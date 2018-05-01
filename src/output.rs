use std::path::Path;
use toml;
use std;
use std::collections::{BTreeMap, BTreeSet};
use std::io::{Read, Write};
use regex::Regex;
use std::path::PathBuf;
use itertools::Itertools;

use {Error, ErrorKind};
use cfg;
use krate::*;
use cache::*;

const PREAMBLE: &'static str = include_str!("preamble.nix");
struct FeatName<'a>(&'a str);
impl<'a> std::fmt::Display for FeatName<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.0.chars().all(|x| {
            (x >= '0' && x <= '9') || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || x == '_'
                || x == '-'
        }) {
            write!(f, "{}", self.0)
        } else {
            write!(f, "\"{}\"", self.0)
        }
    }
}

#[derive(Debug)]
enum CrateType {
    Workspace(BTreeMap<String, (Crate, PathBuf)>),
    Single(String, (Crate, PathBuf)),
}

enum CrateIter<'a> {
    Workspace(std::collections::btree_map::Iter<'a, String, (Crate, PathBuf)>),
    Single(std::iter::Once<(&'a String, &'a (Crate, PathBuf))>),
}

impl<'a> Iterator for CrateIter<'a> {
    type Item = (&'a String, &'a (Crate, PathBuf));
    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            CrateIter::Workspace(ref mut x) => x.next(),
            CrateIter::Single(ref mut x) => x.next(),
        }
    }
}

impl CrateType {
    fn is_workspace(&self) -> bool {
        if let CrateType::Workspace(_) = *self {
            true
        } else {
            false
        }
    }
    fn get(&self, key: &str) -> Option<&(Crate, PathBuf)> {
        match *self {
            CrateType::Workspace(ref x) => x.get(key),
            CrateType::Single(ref key0, ref x) if key == key0 => Some(x),
            CrateType::Single(_, _) => None,
        }
    }
    fn source_type<P: AsRef<Path>>(&self, src: Option<P>, name: &str) -> SourceType {
        if let Some(&(_, ref path)) = self.get(name) {
            if let Some(ref src) = src {
                if self.is_workspace() {
                    SourceType::Path {
                        path: src.as_ref().to_path_buf(),
                        workspace_member: Some(path.to_path_buf()),
                    }
                } else {
                    SourceType::Path {
                        path: src.as_ref().to_path_buf(),
                        workspace_member: None,
                    }
                }
            } else {
                SourceType::CratesIO
            }
        } else {
            SourceType::None
        }
    }
    fn iter(&self) -> CrateIter {
        match *self {
            CrateType::Workspace(ref x) => CrateIter::Workspace(x.iter()),
            CrateType::Single(ref a, ref b) => CrateIter::Single(std::iter::once((a, b))),
        }
    }
}

fn workspace_members(base_path: &Path, cargo_toml: &toml::Value) -> Option<CrateType> {
    if let Some(ws) = cargo_toml.get("workspace") {
        let mut workspace_members = BTreeMap::new();
        let members = ws.as_table()
            .unwrap()
            .get("members")
            .unwrap()
            .as_array()
            .unwrap();
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
                    workspace_members.insert(name.as_str().unwrap().to_owned(), (cra, path));
                }
            }
        }
        Some(CrateType::Workspace(workspace_members))
    } else if let Some(package) = cargo_toml.get("package") {
        let name = package.get("name").unwrap();
        let cra = get_package_version(&package);
        Some(CrateType::Single(
            name.as_str().unwrap().to_owned(),
            (cra, base_path.to_path_buf()),
        ))
    } else {
        None
    }
}

pub fn generate_nix<W: Write, P: AsRef<Path>, Q: AsRef<Path>>(
    lockfile: P,
    standalone: bool,
    src: Option<Q>,
    nix_file: W,
) -> Result<(), Error> {
    let mut cr = lockfile.as_ref().to_path_buf();
    let mut lock: toml::Value = {
        let mut lockfile = std::fs::File::open(&cr).unwrap();
        let mut toml = String::new();
        lockfile.read_to_string(&mut toml).unwrap();
        toml::de::from_str(&toml).unwrap()
    };

    let mut packages = if let Some(package) = lock.as_table_mut().unwrap().remove("package") {
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
            cargo_toml.get("features"),
        )
    };

    let base_path = cr.parent().unwrap();

    let workspace_members =
        workspace_members(base_path, &cargo_toml).expect("neither workspace nor regular crate");
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

    let main_meta = if let Some(root) = lock.as_table_mut().unwrap().get("root") {
        let main_cra = get_package_version(&root);

        debug!("name = {:?}", main_cra.name);
        if let Some(dep) = deps.get(&main_cra.name) {
            debug!("dep = {:?}", dep);
        }

        let path = workspace_members.source_type(src.as_ref(), &main_cra.name);
        let mut meta = main_cra.prefetch(&mut cache, &path).unwrap();
        if let Some(inc) = cargo_toml
            .get("package")
            .unwrap()
            .as_table()
            .unwrap()
            .get("include")
        {
            meta.include = Some(
                inc.as_array()
                    .unwrap()
                    .into_iter()
                    .map(|x| x.as_str().unwrap().to_string())
                    .collect(),
            )
        }
        if let Some(ref src) = src {
            meta.src = Src::Path {
                path: src.as_ref().to_path_buf(),
                workspace_member: None,
            }
        }
        Some(meta)
    } else {
        None
    };

    let mut all_packages = fixpoint(
        &mut cache,
        src,
        &mut packages,
        &mut deps,
        &workspace_members,
        main_meta.as_ref(),
    )?;

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

    output(standalone, workspace_members, all_packages, nix_file)?;
    Ok(())
}

fn cargo_lock_source_type(package: &toml::Value) -> SourceType {
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
}

fn local_source_type<P:AsRef<Path>>(
    crate_type: &CrateType,
    src: Option<P>,
    main_meta: Option<&Meta>,
    deps: &BTreeMap<String, Dep>,
    name: &str,
) -> SourceType {
    let loc = crate_type.source_type(src.as_ref(), name);
    if let SourceType::None = loc {
        if let Some(ref dep) = deps.get(name) {
            if let Some(ref p) = dep.path {
                debug!("path {:?}", p);
                return SourceType::Path {
                    path: p.to_path_buf(),
                    workspace_member: None,
                };
            }
        } else if let Some(ref main_meta) = main_meta {
            for &(_, ref deps) in main_meta.target_dependencies.iter() {
                if let Some(ref dep) = deps.get(name) {
                    if let Some(ref p) = dep.path {
                        debug!("path {:?}", p);
                        return SourceType::Path {
                            path: p.to_path_buf(),
                            workspace_member: None,
                        };
                    }
                }
            }
        }
    }
    loc
}

fn resolve_replaces(packages: &mut Vec<toml::Value>) -> Vec<toml::Value> {
    // resolve all "replace = "
    let mut replace = BTreeMap::new();
    for pack in packages.iter() {
        if let (Some(repl), Some(source)) = (pack.get("replace"), pack.get("source")) {
            replace.insert(
                repl.as_str().unwrap().to_string(),
                source.as_str().unwrap().to_string(),
            );
        }
    }
    let mut fix = Vec::new();
    for mut pack in packages.drain(..) {
        if pack.get("replace").is_none() {
            let s = format!(
                "{} {}",
                pack.get("name").unwrap().as_str().unwrap(),
                pack.get("version").unwrap().as_str().unwrap()
            );
            if let Some(repl) = replace.get(&s) {
                let mut pack = pack.as_table_mut().unwrap();
                pack.insert("source".to_string(), toml::Value::String(repl.clone()));
                debug!("replaced {:?}", pack);
            }
            fix.push(pack)
        }
    }
    fix
}

fn fixpoint<P: AsRef<Path>>(
    cache: &mut Cache,
    src: Option<P>,
    packages: &mut Vec<toml::Value>,
    deps: &mut BTreeMap<String, Dep>,
    crate_type: &CrateType,
    main_meta: Option<&Meta>,
) -> Result<BTreeMap<Crate, Meta>, Error> {
    let mut all_packages = BTreeMap::new();

    // Here we need to compute a fixpoint of dependencies to resolve
    // all sources. This is because local sources are sometimes
    // referenced not only from this package's Cargo.toml, but from
    // this package's (transitive) dependencies, in which case the
    // only way to know where they are is to go down the dependency
    // tree (because they are not in the Cargo.lock).
    let mut packages_fixpoint = resolve_replaces(packages);
    let mut unknown_packages = packages; // empty at this point.

    while !packages_fixpoint.is_empty() {
        let mut at_least_one_resolved = false;
        for package in packages_fixpoint.drain(..) {
            let cra = get_package_version(&package);

            // We first try to resolve the source type using the Cargo.lock.
            let mut source_type = cargo_lock_source_type(&package);
            debug!("name = {:?} {:?}", cra.name, source_type);

            // Then try from local paths.
            if let SourceType::None = source_type {
                source_type = local_source_type(crate_type, src.as_ref(), main_meta, &deps, &cra.name)
            }

            // Finally, we prefetch the package (if available), and
            // set its source field, if the package is local.
            let meta = {
                debug!("path = {:?}", source_type);
                if source_type != SourceType::None {
                    if let Ok(mut prefetch) = cra.prefetch(cache, &source_type) {
                        Some(prefetch)
                    } else {
                        return Err(ErrorKind::PrefetchFailed(cra).into());
                    }
                } else {
                    None
                }
            };

            debug!("meta = {:?}", meta);
            if let Some(mut meta) = meta {
                // If we've successfully retrieved the package's
                // meta-information (i.e. parsed the part of our
                // Cargo.lock related to that package, and the
                // package's own Cargo.toml), we might have learned
                // exact versions of the package's dependencies from
                // the Cargo.lock, and we need to update them.
                at_least_one_resolved = true;
                for (a, b) in meta.dependencies
                    .iter()
                    .chain(meta.build_dependencies.iter())
                {
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
                // Else, add the package back to the fixpoint. If we
                // have learned about other dependencies during this
                // pass of the fixpoint, we might have updated `deps`,
                // so we might be able to resolve this package later.
                if all_packages.get(&cra).is_none() {
                    unknown_packages.push(package)
                } else {
                    error!(
                        "Several packages with the same name and version number {:?}",
                        cra
                    )
                }
            }
        }
        if !at_least_one_resolved {
            error!("Could not resolve some sources");
            std::process::exit(1)
        }
        std::mem::swap(&mut packages_fixpoint, &mut unknown_packages)
    }

    Ok(all_packages)
}

fn output<W: Write>(
    standalone: bool,
    workspace_members: CrateType,
    all_packages: BTreeMap<Crate, Meta>,
    mut nix_file: W,
) -> Result<(), Error> {
    // And output.
    let mut command_line = String::new();
    for arg in std::env::args() {
        if !command_line.is_empty() {
            command_line.push_str(" ");
        }
        command_line.push_str(&arg);
    }
    nix_file.write_all(
        format!(
            "# Generated by carnix {}: {}\n",
            env!("CARGO_PKG_VERSION"),
            command_line
        ).as_bytes(),
    )?;

    if standalone {
        nix_file.write_all(b"with import <nixpkgs> {};\n")?;
    } else {
        nix_file.write_all(b"{ lib, buildPlatform, buildRustCrate, fetchgit }:\n")?;
    }
    nix_file.write_all(PREAMBLE.as_bytes())?;
    let mut names = BTreeSet::new();
    let mut is_first_package = true;
    nix_file.write_all(b"rec {\n  ")?;
    let mut all = b"[ ".to_vec();
    for (_, &(ref cra, _)) in workspace_members.iter() {
        let name = nix_name(&cra.name);
        let full_name = format!(
            "{}_{}_{}_{}{}{}",
            name,
            cra.major,
            cra.minor,
            cra.patch,
            if cra.subpatch.is_empty() { "" } else { "_" },
            nix_name(&cra.subpatch)
        );
        write!(
            nix_file,
            "{} = f: {} {{ features = {}_features {{ {} = f; }}; }};\n  ",
            name, full_name, full_name, full_name
        )?;
        write!(&mut all, "({} {{}}) ", name)?;
    }
    write!(&mut all, "]")?;
    nix_file.write_all(b"__all = ")?;
    nix_file.write_all(&all)?;
    nix_file.write_all(b";\n  ")?;
    for (cra, meta) in all_packages.iter() {
        cra.output_package(&mut nix_file, 2, &meta, is_first_package)?;
        is_first_package = false;
        names.insert(cra.name.clone());
    }
    for (cra, meta) in all_packages.iter() {
        cra.output_package_call(&mut nix_file, 2, &meta, &names)?;
    }
    nix_file.write_all(b"}\n")?;
    Ok(())
}

/// Add the dependencies from Cargo.lock.
fn update_deps(cra: &Crate, deps: &toml::Value, meta: &mut Meta) {
    let dep_re = Regex::new(r"^(\S*) (\d*)\.(\d*)\.(\d*)(-(\S*))?").unwrap();
    let mut deps_names = BTreeSet::new();
    for dep in deps.as_array().unwrap() {
        let dep = dep.as_str().unwrap();
        let cap = dep_re.captures(&dep).unwrap();
        deps_names.insert(cap.get(1).unwrap().as_str());
        debug!("updating deps for {:?}: {:?}", cra.name, dep);
        let (a, b, c, d) = {
            (
                cap.get(2).unwrap().as_str().parse().unwrap(),
                cap.get(3).unwrap().as_str().parse().unwrap(),
                cap.get(4).unwrap().as_str().parse().unwrap(),
                cap.get(6)
                    .map(|x| x.as_str().to_string())
                    .unwrap_or(String::new()),
            )
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
    let (a, b, c, d) = (
        cap.get(1).unwrap().as_str().parse().unwrap(),
        cap.get(2).unwrap().as_str().parse().unwrap(),
        cap.get(3).unwrap().as_str().parse().unwrap(),
        cap.get(5)
            .map(|x| x.as_str().to_string())
            .unwrap_or(String::new()),
    );
    let name = package
        .as_table()
        .unwrap()
        .get("name")
        .unwrap()
        .as_str()
        .unwrap()
        .to_string();
    Crate {
        major: a,
        minor: b,
        patch: c,
        subpatch: d,
        name: name,
        found_in_lock: true,
    }
}

impl Crate {
    pub fn output_package_call<W: Write>(
        &self,
        mut w: W,
        n_indent: usize,
        meta: &Meta,
        all_packages: &BTreeSet<String>,
    ) -> Result<(), Error> {
        let mut indent = String::new();
        for _ in 0..n_indent {
            indent.push(' ');
        }
        let full_name = format!(
            "{}_{}_{}_{}{}{}",
            nix_name(&self.name),
            self.major,
            self.minor,
            self.patch,
            if self.subpatch.is_empty() { "" } else { "_" },
            nix_name(&self.subpatch)
        );
        debug!("output_package_call {:?}", full_name);

        write!(
            w,
            "{}{} = {{ features?({}_features {{}}) }}: {}_ {{",
            indent, full_name, full_name, full_name
        )?;

        let mut is_first = true;
        let mut has_feature_deps = false;
        if !meta.dependencies.is_empty() {
            is_first = false;
            write!(w, "\n{}  dependencies =", indent)?;
            has_feature_deps |= print_deps(
                &mut w,
                &indent,
                &full_name,
                meta.dependencies
                    .iter()
                    .filter(|&(_, c)| {
                        c.cr.found_in_lock && c.cr.name.len() > 0
                            && all_packages.contains(&c.cr.name)
                    })
                    .map(|x| x.1),
            )?;
        }
        if !meta.target_dependencies.is_empty() {
            for &(ref target, ref dep) in meta.target_dependencies.iter() {
                if is_first {
                    write!(w, "\n{}  dependencies = (", indent)?;
                } else {
                    write!(w, "\n{}    ++ (", indent)?;
                }
                debug!("target = {:?}", target);
                let parsed = cfg::parse_target(target)?;
                write!(w, "if ")?;
                cfg::to_nix(&mut w, &parsed)?;
                write!(w, " then")?;
                has_feature_deps |= print_deps(
                    &mut w,
                    &indent,
                    &full_name,
                    dep.iter()
                        .filter(|&(_, c)| {
                            c.cr.found_in_lock && c.cr.name.len() > 0
                                && all_packages.contains(&c.cr.name)
                        })
                        .map(|x| x.1),
                )?;
                write!(w, " else [])")?;
                is_first = false;
            }
        }
        if !meta.dependencies.is_empty() || !meta.target_dependencies.is_empty() {
            write!(w, ";")?;
        }

        if !meta.build_dependencies.is_empty() {
            write!(w, "\n{}  buildDependencies =", indent)?;
            print_deps(
                &mut w,
                &indent,
                &full_name,
                meta.build_dependencies
                    .iter()
                    .filter(|&(_, c)| {
                        c.cr.found_in_lock && c.cr.name.len() > 0
                            && all_packages.contains(&c.cr.name)
                    })
                    .map(|x| x.1),
            )?;
            write!(w, ";")?;
        }
        if !meta.declared_features.is_empty() || has_feature_deps {
            write!(
                w,
                "\n{}  features = mkFeatures (features.{} or {{}});",
                indent, full_name
            )?;
        }
        if !meta.dependencies.is_empty() || !meta.declared_features.is_empty()
            || !meta.target_dependencies.is_empty()
        {
            write!(w, "\n{}", indent)?;
        }
        writeln!(w, "}};")?;

        writeln!(
            w,
            "{}{}_features = f: updateFeatures f (rec {{",
            indent, full_name
        )?;
        let mut output_features = BTreeMap::new();
        let full_name_default = format!("{}.default", full_name);
        if meta.use_default_features == Some(false) {
            // let e = output_features.entry(format!("{}.default", full_name)).or_insert(Vec::new());
            // e.push("false".to_string());
        } else {
            let e = output_features
                .entry(full_name_default.clone())
                .or_insert(Vec::new());
            e.push(format!("(f.{}.default or true)", full_name))
        }
        if !meta.implied_features.is_empty() {
            for feat in meta.implied_features.iter() {
                let dep = format!("{}.{}", full_name, FeatName(&feat.dep_feature));
                let mut e = output_features.entry(dep).or_insert(Vec::new());
                e.push(format!(
                    "(f.{}.{} or false)",
                    full_name,
                    FeatName(&feat.feature)
                ));
                e.push(format!(
                    "({}.{} or false)",
                    full_name,
                    FeatName(&feat.feature)
                ));
            }
        }

        let mut seen = BTreeSet::new();
        let mut default = BTreeMap::new();
        for deps in std::iter::once(&meta.dependencies)
            .chain(std::iter::once(&meta.build_dependencies))
            .chain(meta.target_dependencies.iter().map(|&(_, ref y)| y))
        {
            for (_, dep) in deps.iter().filter(|&(_, dep)| dep.cr.found_in_lock) {
                debug!("outputting dep = {:?}", dep);
                for feat in dep.features.iter() {
                    let dep = format!(
                        "{}_{}_{}_{}{}{}.{}",
                        nix_name(&dep.cr.name),
                        dep.cr.major,
                        dep.cr.minor,
                        dep.cr.patch,
                        if dep.cr.subpatch.is_empty() { "" } else { "_" },
                        nix_name(&dep.cr.subpatch),
                        FeatName(&feat),
                    );
                    let mut e = output_features.entry(dep).or_insert(Vec::new());
                    e.push("true".to_string());
                }

                if !dep.conditional_features.is_empty() {
                    for feat in dep.conditional_features.iter() {
                        if !seen.contains(&(&dep.cr.name, &feat.feature)) {
                            let dep_name = format!(
                                "{}_{}_{}_{}{}{}.{}",
                                nix_name(&dep.cr.name),
                                dep.cr.major,
                                dep.cr.minor,
                                dep.cr.patch,
                                if dep.cr.subpatch.is_empty() { "" } else { "_" },
                                nix_name(&dep.cr.subpatch),
                                FeatName(&feat.dep_feature),
                            );
                            let mut e = output_features.entry(dep_name).or_insert(Vec::new());
                            e.push(format!(
                                "({}.{} or false)",
                                full_name,
                                FeatName(&feat.feature)
                            ));
                            e.push(format!(
                                "(f.{}.{} or false)",
                                full_name,
                                FeatName(&feat.feature)
                            ));
                            seen.insert((&dep.cr.name, &feat.feature));
                        }
                    }
                }
                let dep_name = format!(
                    "{}_{}_{}_{}{}{}",
                    nix_name(&dep.cr.name),
                    dep.cr.major,
                    dep.cr.minor,
                    dep.cr.patch,
                    if dep.cr.subpatch.is_empty() { "" } else { "_" },
                    nix_name(&dep.cr.subpatch)
                );
                if !dep.default_features {
                    if default.get(&dep_name).is_none() {
                        default.insert(dep_name, false);
                    }
                } else {
                    default.insert(dep_name, true);
                }
            }
        }
        for (dep_name, default) in default.iter() {
            let dep = format!("{}.default", dep_name);
            let mut e = output_features.entry(dep).or_insert(Vec::new());
            e.push(format!("{}", *default));
        }

        for (a, b) in output_features.iter() {
            if b.len() == 0 {
            } else if b.len() == 1 {
                if b[0] == "true" || a == &full_name_default {
                    writeln!(w, "{}  {} = {};", indent, a, b[0])?
                } else if b[0] != "false" {
                    writeln!(w, "{}  {} = (f.{} or false) || {};", indent, a, a, b[0])?
                } else if a.ends_with(".default") {
                    // b[0] == false here
                    writeln!(w, "{}  {} = (f.{} or false);", indent, a, a)?
                }
            } else {
                write!(w, "{}  {} =\n{}    (f.{} or false)", indent, a, indent, a)?;
                for bb in b.iter() {
                    write!(w, " ||\n{}    {}", indent, bb)?;
                }
                writeln!(w, ";")?;
            }
        }

        write!(w, "{}}}) [", indent)?;
        let mut at_least_one = false;
        for (_, deps) in std::iter::once(("", &meta.dependencies))
            .chain(std::iter::once(("_build", &meta.build_dependencies)))
            .chain(meta.target_dependencies.iter().map(|&(_, ref y)| ("", y)))
        {
            for (_, dep) in deps.iter().filter(|&(_, c)| {
                c.cr.found_in_lock && c.cr.name.len() > 0 && all_packages.contains(&c.cr.name)
            }) {
                at_least_one = true;
                write!(
                    w,
                    " {}_{}_{}_{}{}{}_features",
                    nix_name(&dep.cr.name),
                    dep.cr.major,
                    dep.cr.minor,
                    dep.cr.patch,
                    if dep.cr.subpatch.is_empty() { "" } else { "_" },
                    nix_name(&dep.cr.subpatch)
                )?
            }
        }
        if at_least_one {
            writeln!(w, " ];")?;
        } else {
            writeln!(w, "];")?;
        }
        Ok(())
    }

    pub fn output_package<W: Write>(
        &self,
        mut w: W,
        n_indent: usize,
        meta: &Meta,
        is_first_package: bool,
    ) -> Result<(), std::io::Error> {
        let mut indent = String::new();
        for _ in 0..n_indent {
            indent.push(' ');
        }
        write!(w, "{}{}_{}_{}_{}{}{}_ = {{ dependencies?[], buildDependencies?[], features?[] }}: buildRustCrate {{\n", if is_first_package { "" } else { &indent }, nix_name(&self.name), self.major, self.minor, self.patch,
               if self.subpatch.is_empty() {""} else {"_"},
               nix_name(&self.subpatch))?;
        // writeln!(w, "mkRustCrate {{")?;
        writeln!(w, "{}  crateName = \"{}\";", indent, self.name)?;
        let version = if self.subpatch.len() > 0 {
            format!(
                "{}.{}.{}-{}",
                self.major, self.minor, self.patch, self.subpatch
            )
        } else {
            format!("{}.{}.{}", self.major, self.minor, self.patch)
        };

        writeln!(w, "{}  version = \"{}\";", indent, version)?;

        writeln!(
            w,
            "{}  authors = [ {} ];",
            indent,
            meta.authors.iter().map(|s| format!("\"{}\"", s)).join(" ")
        )?;

        match meta.src {
            Src::Crate { ref sha256 } => {
                writeln!(w, "{}  sha256 = \"{}\";", indent, sha256)?;
            }
            Src::Path {
                ref path,
                ref workspace_member,
            } => {
                let s = path.to_string_lossy();

                let mut filter_source = String::new();
                if let Some(ref include) = meta.include {
                    filter_source.push_str("include [ ");
                    for file in include.iter() {
                        filter_source.push_str("\"");
                        filter_source.push_str(file);
                        filter_source.push_str("\"");
                        filter_source.push_str(" ");
                    }
                    filter_source.push_str("] ");
                }

                if s.len() > 0 {
                    let mut comp = Vec::new();
                    for c in path.components() {
                        use std::path::Component::*;
                        match c {
                            RootDir => comp.push(RootDir),
                            Prefix(c) => comp.push(Prefix(c)),
                            CurDir => {}
                            ParentDir => {
                                comp.pop();
                            }
                            Normal(c) => comp.push(Normal(c)),
                        }
                    }
                    let mut path = PathBuf::new();
                    path.extend(comp.iter());
                    if path.is_relative() {
                        if comp.len() == 1 {
                            filter_source.push_str("./")
                        } else if comp.len() == 0 {
                            filter_source.push_str("./.")
                        }
                    }
                    filter_source.push_str(&path.to_string_lossy());
                } else {
                    filter_source.push_str("./.");
                }

                writeln!(w, "{}  src = {};", indent, filter_source)?;
                if let Some(ref ws) = *workspace_member {
                    writeln!(
                        w,
                        "{}  workspace_member = \"{}\";",
                        indent,
                        ws.to_string_lossy()
                    )?;
                }
            }
            Src::Git(ref git) => {
                writeln!(w, "{}  src = fetchgit {{", indent)?;
                writeln!(w, "{}     url = {:?};", indent, git.url)?;
                writeln!(w, "{}     rev = {:?};", indent, git.rev)?;
                writeln!(w, "{}     sha256 = {:?};", indent, git.sha256)?;
                if !git.fetch_submodules {
                    writeln!(w, "{}     fetchSubmodules = false;", indent)?;
                }
                writeln!(w, "{}  }};", indent)?;
            }
        }
        if meta.crate_file.len() > 0 {
            writeln!(w, "{}  libPath = \"{}\";", indent, meta.crate_file)?;
        }
        if meta.lib_name.len() > 0 && meta.lib_name != self.name {
            writeln!(w, "{}  libName = \"{}\";", indent, meta.lib_name)?;
        }
        if meta.proc_macro {
            writeln!(w, "{}  procMacro = {};", indent, meta.proc_macro)?;
        }
        if meta.plugin {
            writeln!(w, "{}  plugin = {};", indent, meta.plugin)?;
        }
        if let Some(ref t) = meta.crate_type {
            writeln!(w, "{}  type = {};", indent, t)?;
        }
        if meta.bins.len() > 0 {
            write!(w, "{}  crateBin = [ ", indent)?;
            for bin in meta.bins.iter() {
                write!(w, "{{ ")?;
                if let Some(ref name) = bin.name {
                    write!(w, " name = \"{}\"; ", name)?;
                }
                if let Some(ref path) = bin.path {
                    write!(w, " path = \"{}\"; ", path)?;
                }
                write!(w, "}} ")?;
            }
            writeln!(w, "];")?;
        }
        if meta.build.len() > 0 {
            writeln!(w, "{}  build = \"{}\";", indent, meta.build)?;
        }
        writeln!(
            w,
            "{}  inherit dependencies buildDependencies features;",
            indent
        )?;
        writeln!(w, "{}}};", indent)?;
        Ok(())
    }
}

fn print_deps<'a, W: Write, I: Iterator<Item = &'a Dep>>(
    mut w: W,
    indent: &str,
    full_name: &str,
    deps: I,
) -> Result<bool, std::io::Error> {
    write!(w, " mapFeatures features ([")?;
    let mut at_least_one = false;
    let mut feature_deps = Vec::new();
    for i in deps {
        at_least_one = true;
        debug!("print_deps i {:?}", i);
        if i.is_optional {
            feature_deps.push(i)
        } else {
            write!(
                w,
                " {}_{}_{}_{}{}{}",
                nix_name(&i.cr.name),
                i.cr.major,
                i.cr.minor,
                i.cr.patch,
                if i.cr.subpatch.is_empty() { "" } else { "_" },
                nix_name(&i.cr.subpatch)
            )?;
        }
    }

    if at_least_one {
        write!(w, " ]")?;
    } else {
        write!(w, "]")?;
    }
    for i in feature_deps.iter() {
        write!(
            w,
            "\n{}    ++ (if features.{}.{} or false then [ {}_{}_{}_{} ] else [])",
            indent,
            full_name,
            FeatName(&i.cr.name),
            nix_name(&i.cr.name),
            i.cr.major,
            i.cr.minor,
            i.cr.patch
        )?;
    }
    write!(w, ")")?;
    Ok(!feature_deps.is_empty())
}
