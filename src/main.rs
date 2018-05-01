extern crate itertools;
extern crate toml;
extern crate regex;
extern crate tempdir;
#[macro_use]
extern crate clap;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate rusqlite;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate nom;
use regex::Regex;
use std::io::{Read, Write, BufWriter};
use clap::{Arg, App};
use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;
use std::process::Command;

error_chain!{
    foreign_links {
        Io(::std::io::Error);
        Utf8(::std::string::FromUtf8Error);
        Toml(::toml::de::Error);
    }
    errors {
        CouldNotTranslateTarget{}
        NixPrefetchGitFailed{
            description("nix-prefetch-git failed")
        }
    }
}


mod cache;
use cache::*;

mod krate;
use krate::*;
mod cfg;

const PREAMBLE:&'static str = include_str!("preamble.nix");

fn main() {
    env_logger::init();
    let version = crate_version!();
    let matches = App::new("carnix")
        .version(version)
        .author("pmeunier <pe@pijul.org>")
        .about("Generate a nix derivation set from a cargo registry")
        .arg(Arg::with_name("lockfile")
             .value_name("LOCKFILE")
             .help("Input Cargo.lock file")
             .required(true)
             .takes_value(true))
        .arg(Arg::with_name("src")
             .long("--src")
             .help("Source of the main project")
             .takes_value(true))
        .arg(Arg::with_name("standalone")
             .long("--standalone")
             .help("Produce a standalone file, which can be built directly with nix-build."))
        .arg(Arg::with_name("target")
             .value_name("TARGET")
             .long("--output")
             .short("-o")
             .help("Output of this command (a nix file)")
             .takes_value(true))
        .get_matches();

    if ! can_find_nix_prefetch_url() {
        error!("Cannot find executable: nix-prefetch-url.  Does $PATH include the executable's location?");
        std::process::exit(1);
    }

    let mut cr = Path::new(matches.value_of("lockfile").unwrap()).to_path_buf();
    let mut lock: toml::Value = {
        let mut lockfile = std::fs::File::open(&cr).unwrap();
        let mut toml = String::new();
        lockfile.read_to_string(&mut toml).unwrap();
        toml::de::from_str(&toml).unwrap()
    };

    let packages = if let Some(package) = lock.as_table_mut().unwrap().remove("package") {
        if let toml::Value::Array(package) = package {
            package
        } else {
            Vec::new()
        }
    } else {
        Vec::new()
    };

    cr.set_extension("toml");

    let cargo_toml: toml::Value = {
        let mut cargofile = std::fs::File::open(&cr).unwrap();
        let mut toml = String::new();
        cargofile.read_to_string(&mut toml).unwrap();
        toml::de::from_str(&toml).unwrap()
    };

    let (mut deps, _) = {
        make_dependencies(
            cr.parent().unwrap(),
            cargo_toml.get("dependencies"),
            cargo_toml.get("features")
        )
    };

    let mut workspace_members = BTreeMap::new();

    let base_path = cr.parent().unwrap();
    let mut is_workspace = false;
    if let Some(ws) = cargo_toml.get("workspace") {
        is_workspace = true;
        let members = ws.as_table().unwrap().get("members").unwrap().as_array().unwrap();
        for mem in members {
            let mem = mem.as_str().unwrap();
            let mut path = base_path.join(mem);
            path.push("Cargo.toml");
            let mut toml: toml::Value = {
                let mut cargofile = std::fs::File::open(&path).unwrap();
                let mut toml = String::new();
                cargofile.read_to_string(&mut toml).unwrap();
                toml::de::from_str(&toml).unwrap()
            };
            if let Some(package) = toml.as_table_mut().unwrap().remove("package") {
                {
                    let name = package.get("name").unwrap();
                    let cra = get_package_version(&package);
                    debug!("mem path {:?}", path);
                    path.pop();
                    workspace_members.insert(
                        name.as_str().unwrap().to_owned(),
                        (cra, path)
                    );
                }
            }
        }
    } else if let Some(package) = cargo_toml.get("package") {
        let name = package.get("name").unwrap();
        let cra = get_package_version(&package);
        workspace_members.insert(
            name.as_str().unwrap().to_owned(),
            (cra, base_path.to_path_buf())
        );
    }
    debug!("ws members {:?}", workspace_members);
    debug!("main deps: {:?} {:?}", cr, deps);


    // Loading the cache (a map between package versions and Nix store paths).
    let mut cache_path = std::env::home_dir().unwrap();
    cache_path.push(".cargo");
    std::fs::create_dir_all(&cache_path).unwrap();
    cache_path.push("nix-cache");
    let mut cache = Cache::new(&cache_path);

    // Compute the meta-information for all packages in the lock file.
    debug!("base_path: {:?}", base_path);


    let main_meta =
        if let Some(root) = lock.as_table_mut().unwrap().get("root") {
            let main_cra = get_package_version(&root);

            debug!("name = {:?}", main_cra.name);
            if let Some(dep) = deps.get(&main_cra.name) {
                debug!("dep = {:?}", dep);
            }
            let path =
                if let Some(&(_, ref path)) = workspace_members.get(&main_cra.name) {
                    SourceType::Path {
                        path: base_path.to_path_buf(),
                        workspace_member: Some(path.to_path_buf())
                    }
                } else {
                    SourceType::Path { path: base_path.to_path_buf(), workspace_member: None }
                };
            let mut meta = main_cra.prefetch(&mut cache, &path).unwrap();
            if let Some(src) = matches.value_of("src") {
                meta.src = Src::Path {
                    path: Path::new(src).to_path_buf(),
                    workspace_member: None,
                }
            }
            Some(meta)
        } else {
            None
        };





    let mut all_packages = BTreeMap::new();

    let mut unknown_packages = Vec::new();

    // Here we need to compute a fixpoint of dependencies to resolve
    // all sources. This is because local sources are sometimes
    // referenced not only from this package's Cargo.toml, but from
    // this package's (transitive) dependencies, in which case the
    // only way to know where they are is to go down the dependency
    // tree (because they are not in the Cargo.lock).
    let mut packages_fixpoint = packages.clone();
    {
        // resolve all "replace = "
        let mut replace = BTreeMap::new();
        for pack in packages_fixpoint.iter() {
            if let (Some(repl), Some(source)) = (pack.get("replace"), pack.get("source")) {
                replace.insert(
                    repl.as_str().unwrap().to_string(),
                    source.as_str().unwrap().to_string()
                );
            }
        }
        let mut fix = Vec::new();
        for mut pack in packages_fixpoint.drain(..) {
            if pack.get("replace").is_none() {
                let s = format!(
                    "{} {}",
                    pack.get("name").unwrap().as_str().unwrap(),
                    pack.get("version").unwrap().as_str().unwrap()
                );
                if let Some(repl) = replace.get(&s) {
                    let mut pack = pack.as_table_mut().unwrap();
                    pack.insert(
                        "source".to_string(),
                        toml::Value::String(repl.clone())
                    );
                    debug!("replaced {:?}", pack);

                }
                fix.push(pack)
            }
        }
        packages_fixpoint = fix
    }


    while !packages_fixpoint.is_empty() {

        let mut at_least_one_resolved = false;
        for package in packages_fixpoint.drain(..) {

            let cra = get_package_version(&package);

            let mut source_type = {
                if let Some(source) = package.get("source") {
                    let source = source.as_str().unwrap();
                    if source == "registry+https://github.com/rust-lang/crates.io-index" {
                        SourceType::CratesIO
                    } else if source.starts_with("git+") {
                        let url = source.split_at(4).1;
                        debug!("url = {:?}", url);
                        let mut commit = url.split('#');
                        if let (Some(url), Some(rev)) = (commit.next(), commit.next()) {
                            SourceType::Git {
                                url: url.to_string(),
                                rev: rev.to_string(),
                            }
                        } else {
                            error!("could not parse Git url");
                            SourceType::None
                        }
                    } else {
                        SourceType::None
                    }
                } else {
                    SourceType::None
                }
            };
            debug!("name = {:?} {:?}", cra.name, source_type);

            let meta = {
                // debug!("deps: {:?}", deps);
                let is_src = if let Some(&(_, ref path)) = workspace_members.get(&cra.name) {

                    if let SourceType::None = source_type {
                        if is_workspace {
                            source_type = SourceType::Path {
                                path: base_path.to_path_buf(),
                                workspace_member: Some(path.to_path_buf()),
                            }
                        } else {
                            source_type = SourceType::Path {
                                path: path.to_path_buf(),
                                workspace_member: None,
                            }
                        }
                    }
                    !is_workspace

                } else if let Some(ref dep) = deps.get(&cra.name) {

                    if let Some(ref p) = dep.path {
                        debug!("path {:?}", p);
                        if let SourceType::None = source_type {
                            source_type = SourceType::Path {
                                path: p.to_path_buf(),
                                workspace_member: None,
                            };
                        }
                    }
                    false

                } else if let Some(ref main_meta) = main_meta {

                    for &(_, ref deps) in main_meta.target_dependencies.iter() {
                        if let Some(ref dep) = deps.get(&cra.name) {
                            if let Some(ref p) = dep.path {
                                debug!("path {:?}", p);
                                if let SourceType::None = source_type {
                                    source_type = SourceType::Path {
                                        path: p.to_path_buf(),
                                        workspace_member: None,
                                    };
                                }
                                break
                            }
                        }
                    }
                    false

                } else {

                    false

                };
                debug!("path = {:?}", source_type);
                if source_type != SourceType::None {
                    if let Ok(mut prefetch) = cra.prefetch(&mut cache, &source_type) {
                        if is_src {
                            if let Some(src) = matches.value_of("src") {
                                prefetch.src = Src::Path {
                                    path: Path::new(src).to_path_buf(),
                                    workspace_member: None,
                                }
                            }
                        }
                        Some(prefetch)
                    } else {
                        return
                    }
                } else {
                    None
                }
            };

            debug!("meta = {:?}", meta);
            if let Some(mut meta) = meta {
                at_least_one_resolved = true;
                for (a, b) in meta.dependencies.iter().chain(meta.build_dependencies.iter()) {
                    debug!("inserting dep for {:?}: {:?}", a, b);
                    deps.insert(a.to_string(), b.clone());
                }

                if let Some(deps) = package.get("dependencies") {
                    update_deps(&cra, deps, &mut meta)
                }
                if let Some(deps) = package.get("build-dependencies") {
                    update_deps(&cra, deps, &mut meta)
                }
                if let Some(target) = package.get("target") {
                    for package in target.as_array().unwrap().iter() {
                        if let Some(deps) = package.get("dependencies") {
                            update_deps(&cra, deps, &mut meta)
                        }
                    }
                }

                all_packages.insert(cra, meta);
            } else {
                if all_packages.get(&cra).is_none() {
                    unknown_packages.push(package)
                } else {
                    error!("Several packages with the same name and version number {:?}", cra)
                }
            }
        }
        if !at_least_one_resolved {
            error!("Could not resolve some sources");
            std::process::exit(1)
        }
        std::mem::swap(&mut packages_fixpoint, &mut unknown_packages)
    }

    // Adding the root crate, the one we're compiling.
    if let Some(mut main_meta) = main_meta {
        if let Some(root) = lock.as_table_mut().unwrap().get("root") {
            debug!("root = {:?}", root);
            let main_cra = get_package_version(&root);
            // Add the crates' dependencies.
            if let Some(deps) = root.get("dependencies") {
                update_deps(&main_cra, deps, &mut main_meta);
            }
            if let Some(deps) = root.get("build-dependencies") {
                update_deps(&main_cra, deps, &mut main_meta);
            }
            all_packages.insert(main_cra.clone(), main_meta);
        }
    }

    // And output.
    let mut nix_file:Box<Write> = if let Some(nix) = matches.value_of("target") {
        Box::new(BufWriter::new(std::fs::File::create(nix).unwrap()))
    } else {
        Box::new(BufWriter::new(std::io::stdout()))
    };
    let mut command_line = String::new();
    for arg in std::env::args() {
        if ! command_line.is_empty() {
            command_line.push_str(" ");
        }
        command_line.push_str(&arg);
    }
    nix_file.write_all(format!("# Generated by carnix {}: {}\n", env!("CARGO_PKG_VERSION"), command_line).as_bytes()).unwrap();
    if matches.is_present("standalone") {
        nix_file.write_all(b"with import <nixpkgs> {};\n").unwrap();
    } else {
        nix_file.write_all(b"{ lib, buildPlatform, buildRustCrate, fetchgit }:\n").unwrap();
    }
    nix_file.write_all(PREAMBLE.as_bytes()).unwrap();
    let mut names = BTreeSet::new();
    let mut is_first_package = true;
    nix_file.write_all(b"rec {\n  ").unwrap();
    for (_, &(ref cra, _)) in workspace_members.iter() {
        let name = nix_name(&cra.name);
        let full_name = format!(
            "{}_{}_{}_{}{}{}",
            name,
            cra.major, cra.minor, cra.patch,
            if cra.subpatch.is_empty() {""} else {"_"},
            nix_name(&cra.subpatch)
        );
        write!(nix_file, "{} = f: {} {{ features = {}_features {{ {} = f; }}; }};\n  ",
               name, full_name, full_name, full_name).unwrap();
    }
    for (cra, meta) in all_packages.iter() {
        cra.output_package(&mut nix_file, 2, &meta, is_first_package).unwrap();
        is_first_package = false;
        names.insert(cra.name.clone());
    }
    for (cra, meta) in all_packages.iter() {
        cra.output_package_call(&mut nix_file, 2, &meta, &names).unwrap();
    }
    nix_file.write_all(b"}\n").unwrap();
}

/// Add the dependencies from Cargo.lock.
fn update_deps(cra: &Crate, deps:&toml::Value, meta: &mut Meta)  {
    let dep_re = Regex::new(r"^(\S*) (\d*)\.(\d*)\.(\d*)(-(\S*))?").unwrap();
    let mut deps_names = BTreeSet::new();
    for dep in deps.as_array().unwrap() {
        let dep = dep.as_str().unwrap();
        let cap = dep_re.captures(&dep).unwrap();
        deps_names.insert(cap.get(1).unwrap().as_str());
        debug!("updating deps for {:?}: {:?}", cra.name, dep);
        let (a, b, c, d) = {
            (cap.get(2).unwrap().as_str().parse().unwrap(),
             cap.get(3).unwrap().as_str().parse().unwrap(),
             cap.get(4).unwrap().as_str().parse().unwrap(),
             cap.get(6).map(|x| x.as_str().to_string()).unwrap_or(String::new()))
        };
        let name = cap.get(1).unwrap().as_str().to_string();
        debug!("update name = {:?}", name);
        if let Some(entry) = meta.dependencies.get_mut(&name) {
            debug!("meta.dependencies");
            entry.cr.major = a;
            entry.cr.minor = b;
            entry.cr.patch = c;
            entry.cr.subpatch = d.clone();
            entry.cr.name = name.clone();
            entry.cr.found_in_lock = true;
        }
        if let Some(entry) = meta.build_dependencies.get_mut(&name) {
            debug!("meta.build_dependencies");
            entry.cr.major = a;
            entry.cr.minor = b;
            entry.cr.patch = c;
            entry.cr.subpatch = d.clone();
            entry.cr.name = name.clone();
            entry.cr.found_in_lock = true;
        }
        for &mut (_, ref mut deps) in meta.target_dependencies.iter_mut() {
            if let Some(entry) = deps.get_mut(&name) {
                debug!("meta.target_dependencies");
                entry.cr.major = a;
                entry.cr.minor = b;
                entry.cr.patch = c;
                entry.cr.subpatch = d.clone();
                entry.cr.name = name.clone();
                entry.cr.found_in_lock = true;
            }
        }
    }
}

fn get_package_version(package: &toml::Value) -> Crate {
    let ver_re = Regex::new(r"(\d*)\.(\d*)\.(\d*)(-(\S*))?").unwrap();
    let version = package.get("version").unwrap().as_str().unwrap();
    let cap = ver_re.captures(&version).unwrap();
    let (a, b, c, d) =
        (cap.get(1).unwrap().as_str().parse().unwrap(),
         cap.get(2).unwrap().as_str().parse().unwrap(),
         cap.get(3).unwrap().as_str().parse().unwrap(),
         cap.get(5).map(|x| x.as_str().to_string()).unwrap_or(String::new()));
    let name = package.as_table().unwrap().get("name").unwrap().as_str().unwrap().to_string();
    Crate {
        major: a, minor: b, patch: c, subpatch: d,
        name: name, found_in_lock: true
    }
}

fn can_find_nix_prefetch_url() -> bool {
    Command::new("nix-prefetch-url")
        .args(&[ "--help" ][..])
        .env("PAGER", "echo")
        .status()
        .is_ok()
}
