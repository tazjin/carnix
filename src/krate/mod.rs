use itertools::Itertools;
use std::io::Write;
use std::collections::{BTreeMap, BTreeSet};
use std;
use toml;
use std::path::{Path, PathBuf};
use std::str::from_utf8;
use regex::Regex;
use std::process::Command;
use cache::{Prefetch, Cache};
use {Error, ErrorKind};
mod prefetch;
use cfg;
use serde_json;

#[derive(Debug, Clone, PartialEq, Eq, Default, PartialOrd, Ord)]
pub struct Crate {
    pub name: String,
    pub major: usize,
    pub minor: usize,
    pub patch: usize,
    pub subpatch: String,
    pub found_in_lock: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Meta {
    pub src: Src,
    pub include: Option<Vec<String>>,
    pub dependencies: BTreeMap<String, Dep>,
    pub declared_dependencies: BTreeSet<String>,
    pub target_dependencies: Vec<(String, BTreeMap<String, Dep>)>,
    pub build_dependencies: BTreeMap<String, Dep>,
    pub crate_file: String,
    pub lib_name: String,
    pub proc_macro: bool,
    pub plugin: bool,
    pub crate_type: Option<String>,
    pub default_features: Vec<String>,
    pub declared_features: BTreeSet<String>,
    pub use_default_features: Option<bool>,
    pub build: String,
    pub features: BTreeSet<String>,
    pub implied_features: Vec<ConditionalFeature>,
    pub bins: Vec<Bin>,
    pub authors: Vec<String>,
}

#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct Dep {
    pub cr: Crate,
    pub is_optional: bool,
    pub path: Option<PathBuf>,
    pub features: Vec<String>,
    pub default_features: bool,
    pub conditional_features: Vec<ConditionalFeature>,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub enum Src {
    Crate { sha256: String },
    Path { path: PathBuf },
    Git(GitFetch),
}

#[derive(Debug, PartialEq, Eq)]
pub enum SourceType {
    CratesIO,
    None,
    Git { url: String, rev: String },
    Path { path: PathBuf }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ConditionalFeature {
    pub feature: String,
    pub dep_feature: String,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Bin {
    pub path: Option<String>,
    pub name: Option<String>,
}

use std::str::FromStr;
impl FromStr for Crate {
    type Err = ();
    fn from_str(s: &str) -> Result<Crate, Self::Err> {
        let re = Regex::new(r"(\S*)-(\d*)\.(\d*)\.(\d*)(-(\S*))?").unwrap();
        let cap = re.captures(s).unwrap();
        Ok(Crate {
            name: cap.get(1).unwrap().as_str().to_string(),
            major: cap.get(2).unwrap().as_str().parse().unwrap(),
            minor: cap.get(3).unwrap().as_str().parse().unwrap(),
            patch: cap.get(4).unwrap().as_str().parse().unwrap(),
            subpatch: cap.get(6).map(|x| x.as_str().to_string()).unwrap_or(String::new()),
            found_in_lock: true,
        })
    }
}

struct FeatName<'a>(&'a str);
impl<'a> std::fmt::Display for FeatName<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.0.chars().all(|x| (x >= '0' && x <= '9') || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || x == '_' || x == '-') {
            write!(f, "{}", self.0)
        } else {
            write!(f, "\"{}\"", self.0)
        }
    }
}


pub fn nix_name(name: &str) -> String {
    name.chars().map(|c| match c {
        '-' => '_',
        '.' => '_',
        c => c
    }).collect()
}


fn print_deps<'a, W:Write, I:Iterator<Item = &'a Dep>>(mut w: W, indent: &str, full_name: &str, deps: I) -> Result<bool, std::io::Error> {
    write!(w, " mapFeatures features ([")?;
    let mut at_least_one = false;
    let mut feature_deps = Vec::new();
    for i in deps {
        at_least_one = true;
        debug!("print_deps i {:?}", i);
        if i.is_optional {
            feature_deps.push(i)
        } else {
            write!(w, " {}_{}_{}_{}{}{}", nix_name(&i.cr.name), i.cr.major, i.cr.minor, i.cr.patch,
                   if i.cr.subpatch.is_empty() {""} else {"_"},
                   nix_name(&i.cr.subpatch))?;
        }
    }

    if at_least_one {
        write!(w, " ]")?;
    } else {
        write!(w, "]")?;
    }
    for i in feature_deps.iter() {
        write!(w, "\n{}    ++ (if features.{}.{} or false then [ {}_{}_{}_{} ] else [])",
               indent, full_name, FeatName(&i.cr.name), nix_name(&i.cr.name), i.cr.major, i.cr.minor, i.cr.patch)?;
    }
    write!(w, ")")?;
    Ok(!feature_deps.is_empty())
}


impl Crate {

    pub fn output_package_call<W:Write>(&self, mut w: W, n_indent: usize, meta: &Meta, all_packages: &BTreeSet<String>) -> Result<(), Error> {
        let mut indent = String::new();
        for _ in 0..n_indent {
            indent.push(' ');
        }
        let full_name = format!("{}_{}_{}_{}{}{}", nix_name(&self.name), self.major, self.minor, self.patch,
                                if self.subpatch.is_empty() {""} else {"_"}, nix_name(&self.subpatch));
        debug!("output_package_call {:?}", full_name);

        write!(w, "{}{} = {{ features?({}_features {{}}) }}: {}_ {{", indent, full_name, full_name, full_name)?;

        let mut is_first = true;
        let mut has_feature_deps = false;
        if !meta.dependencies.is_empty() {
            is_first = false;
            write!(w, "\n{}  dependencies =", indent)?;
            has_feature_deps |=
                print_deps(&mut w, &indent, &full_name, meta.dependencies.iter()
                           .filter(|&(_, c)|
                                   c.cr.found_in_lock &&
                                   c.cr.name.len() > 0 &&
                                   all_packages.contains(&c.cr.name)
                           )
                           .map(|x| x.1))?;
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
                has_feature_deps |=
                    print_deps(&mut w, &indent, &full_name, dep.iter()
                               .filter(|&(_, c)|
                                       c.cr.found_in_lock &&
                                       c.cr.name.len() > 0 &&
                                       all_packages.contains(&c.cr.name)
                               )
                               .map(|x| x.1))?;
                write!(w, " else [])")?;
                is_first = false;
            }
        }
        if !meta.dependencies.is_empty() || !meta.target_dependencies.is_empty() {
            write!(w, ";")?;
        }

        if !meta.build_dependencies.is_empty() {
            write!(w, "\n{}  buildDependencies =", indent)?;
            print_deps(&mut w, &indent, &full_name, meta.build_dependencies.iter()
                       .filter(|&(_, c)|
                               c.cr.found_in_lock
                               && c.cr.name.len() > 0
                               && all_packages.contains(&c.cr.name)
                       )
                       .map(|x| x.1))?;
            write!(w, ";")?;
        }
        if !meta.declared_features.is_empty() || has_feature_deps {
            write!(w, "\n{}  features = mkFeatures (features.{} or {{}});", indent, full_name)?;
        }
        if !meta.dependencies.is_empty() || !meta.declared_features.is_empty() || !meta.target_dependencies.is_empty() {
            write!(w, "\n{}", indent)?;
        }
        writeln!(w, "}};")?;

        writeln!(w, "{}{}_features = f: updateFeatures f (rec {{",
                 indent, full_name)?;
        let mut output_features = BTreeMap::new();
        let full_name_default = format!("{}.default", full_name);
        if meta.use_default_features == Some(false) {
            // let e = output_features.entry(format!("{}.default", full_name)).or_insert(Vec::new());
            // e.push("false".to_string());
        } else {
            let e = output_features.entry(full_name_default.clone()).or_insert(Vec::new());
            e.push(format!("(f.{}.default or true)", full_name))
        }
        if !meta.implied_features.is_empty() {
            for feat in meta.implied_features.iter() {
                let dep = format!("{}.{}", full_name, FeatName(&feat.dep_feature));
                let mut e = output_features.entry(dep).or_insert(Vec::new());
                e.push(format!("(f.{}.{} or false)", full_name, FeatName(&feat.feature)));
                e.push(format!("({}.{} or false)", full_name, FeatName(&feat.feature)));
            }
        }

        let mut seen = BTreeSet::new();
        let mut default = BTreeMap::new();
        for deps in
            std::iter::once(&meta.dependencies)
            .chain(std::iter::once(&meta.build_dependencies))
            .chain(meta.target_dependencies.iter().map(|&(_, ref y)| y))
        {
            for (_, dep) in deps.iter().filter(|&(_, dep)| dep.cr.found_in_lock) {
                debug!("outputting dep = {:?}", dep);
                for feat in dep.features.iter() {
                    let dep = format!(
                        "{}_{}_{}_{}{}{}.{}",
                        nix_name(&dep.cr.name), dep.cr.major, dep.cr.minor,
                        dep.cr.patch,
                        if dep.cr.subpatch.is_empty() {""} else {"_"},
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
                                dep.cr.major, dep.cr.minor,
                                dep.cr.patch,
                                if dep.cr.subpatch.is_empty() {""} else {"_"},
                                nix_name(&dep.cr.subpatch),
                                FeatName(&feat.dep_feature),
                            );
                            let mut e = output_features.entry(dep_name).or_insert(Vec::new());
                            e.push(format!("({}.{} or false)", full_name, FeatName(&feat.feature)));
                            e.push(format!("(f.{}.{} or false)", full_name, FeatName(&feat.feature)));
                            seen.insert((&dep.cr.name, &feat.feature));
                        }
                    }
                }
                let dep_name = format!(
                    "{}_{}_{}_{}{}{}",
                    nix_name(&dep.cr.name), dep.cr.major, dep.cr.minor,
                    dep.cr.patch,
                    if dep.cr.subpatch.is_empty() {""} else {"_"},
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
                };
                writeln!(w, ";")?;
            }
        }

        write!(w, "{}}}) [", indent)?;
        let mut at_least_one = false;
        for (_, deps) in
            std::iter::once(("", &meta.dependencies))
            .chain(std::iter::once(("_build", &meta.build_dependencies)))
            .chain(meta.target_dependencies.iter().map(|&(_, ref y)| ("", y)))
        {
            for (_, dep) in deps.iter().filter(|&(_, c)| {
                c.cr.found_in_lock &&
                    c.cr.name.len() > 0 &&
                    all_packages.contains(&c.cr.name)
            })
            {
                at_least_one = true;
                write!(w, " {}_{}_{}_{}{}{}_features",
                       nix_name(&dep.cr.name), dep.cr.major, dep.cr.minor,
                       dep.cr.patch,
                       if dep.cr.subpatch.is_empty() {""} else {"_"},
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

    pub fn output_package<W:Write>(&self, mut w: W, n_indent: usize, meta: &Meta, is_first_package: bool) -> Result<(), std::io::Error> {
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
            format!("{}.{}.{}-{}", self.major, self.minor, self.patch, self.subpatch)
        } else {
            format!("{}.{}.{}", self.major, self.minor, self.patch)
        };

        writeln!(w, "{}  version = \"{}\";", indent, version)?;

        writeln!(w, "{}  authors = [ {} ];", indent, meta.authors.iter().map(|s| format!("\"{}\"", s)).join(" "))?;

        match meta.src {
            Src::Crate { ref sha256 } => {
                writeln!(w, "{}  sha256 = \"{}\";", indent, sha256)?;
            }
            Src::Path { ref path } => {
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
                    if s.chars().any(|c| c == '/') {
                        filter_source.push_str(&s)
                    } else {
                        filter_source.push_str("./");
                        filter_source.push_str(&s);
                    }
                } else {
                    filter_source.push_str("./.");
                }

                writeln!(w, "{}  src = {};", indent, filter_source)?
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
        writeln!(w, "{}  inherit dependencies buildDependencies features;", indent)?;
        writeln!(w, "{}}};", indent)?;
        Ok(())
    }

    fn prefetch_path(&self, cache: &mut Cache) -> Result<Prefetch, std::io::Error> {

        let version = if self.subpatch.len() > 0 {
            format!("{}.{}.{}-{}", self.major, self.minor, self.patch, self.subpatch)
        } else {
            format!("{}.{}.{}", self.major, self.minor, self.patch)
        };
        let url = format!("https://crates.io/api/v1/crates/{}/{}/download", self.name, version);

        let from_cache = cache.get(&url);
        if let Some(ref prefetch) = from_cache {
            if std::fs::metadata(&prefetch.path).is_ok() {
                return Ok(prefetch.clone())
            }
        }

        println!("Prefetching {}-{}", self.name, version);
        debug!("url = {:?}", url);
        let prefetch = Command::new("nix-prefetch-url")
            .args(&[ &url, "--unpack", "--name", &(self.name.clone() + "-" + &version) ][..])
            .output()?;

        let sha256:String = from_utf8(&prefetch.stdout).unwrap().trim().to_string();
        let path = get_path(&prefetch.stderr);
        let pre = Prefetch {
            prefetch: Src::Crate { sha256 },
            path: Path::new(path).to_path_buf(),
        };
        if from_cache.is_none() {
            cache.insert(&url, &pre);
        }
        Ok(pre)
    }




    fn prefetch_git(&self, url: &str, rev: &str, cache: &mut Cache) -> Result<Prefetch, Error> {
        let cached_url = format!("git+{}#{}", url, rev);
        let from_cache = cache.get(&cached_url);
        if let Some(ref prefetch) = from_cache {
            if std::fs::metadata(&prefetch.path).is_ok() {
                return Ok(prefetch.clone())
            }
        }

        println!("Prefetching {} ({})", self.name, cached_url);
        debug!("cached_url = {:?}", cached_url);
        let prefetch = Command::new("nix-prefetch-git")
            .args(&[ "--url", url, "--rev", rev ])
            .output()?;

        if prefetch.status.success() {

            let prefetch_json: GitFetch = serde_json::from_str(from_utf8(&prefetch.stdout).unwrap()).unwrap();
            let path = get_path(&prefetch.stderr);

            let pre = Prefetch {
                prefetch: Src::Git(prefetch_json),
                path: Path::new(path).to_path_buf()
            };
            if from_cache.is_none() {
                cache.insert(&cached_url, &pre);
            }
            Ok(pre)
        } else {
            error!("nix-prefetch-git exited with error code {:?}: {:?}", prefetch.status, prefetch.stderr);
            Err(ErrorKind::NixPrefetchGitFailed.into())
        }
    }
}

fn get_path(stderr: &[u8]) -> &str {
    debug!("{:?}", from_utf8(&stderr));
    let path_re = Regex::new("path is (‘|')?([^’'\n]*)(’|')?").unwrap();
    let prefetch_stderr = from_utf8(&stderr).expect("stderr of nix-prefetch-url not utf8");
    let cap = if let Some(cap) = path_re.captures(prefetch_stderr) {
        cap
    } else {
        eprintln!("nix-prefetch-url returned:\n{}", prefetch_stderr);
        std::process::exit(1)
    };
    cap.get(2).unwrap().as_str()
}


#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GitFetch {
    url: String,
    rev: String,
    date: String,
    sha256: String,
    #[serde(rename = "fetchSubmodules")]
    fetch_submodules: bool
}

fn make_deps(base_path: &Path, deps: &toml::Value) -> BTreeMap<String, Dep> {
    let mut dependencies = BTreeMap::new();
    for (name, dep) in deps.as_table().unwrap() {
        debug!("name={:?}, dep={:?}", name, dep);
        if let Some(dep) = dep.as_table() {
            let name = name.to_string();
            let enabled_features = if let Some(features) = dep.get("features") {
                features
                    .as_array().unwrap()
                    .into_iter()
                    .map(|x| x.as_str().unwrap().to_string())
                    .collect()
            } else {
                Vec::new()
            };
            let path = if let (Some(path), None) = (dep.get("path"), dep.get("version")) {
                Some(path.as_str().unwrap().to_string())
            } else {
                None
            };
            let mut dep = Dep {
                cr: Crate::default(),
                path: if let Some(path) = path {
                    Some(base_path.join(path))
                } else {
                    None
                },
                is_optional: dep.get("optional").map(|x| x.as_bool().unwrap()).unwrap_or(false),
                features: enabled_features,
                default_features: dep.get("default-features").map(|x| x.as_bool().unwrap()).unwrap_or(true),
                conditional_features: Vec::new(),
            };
            dep.cr.name.push_str(&name);
            dependencies.insert(name, dep);
        } else if dep.as_str().is_some() {
            let name = name.to_string();
            let mut dep = Dep {
                cr: Crate::default(),
                path: None,
                features: Vec::new(),
                is_optional: false,
                default_features: true,
                conditional_features: Vec::new(),
            };
            dep.cr.name.push_str(&name);
            dependencies.insert(name, dep);
        }
    }
    dependencies
}

pub fn make_dependencies(base_path: &Path, deps: Option<&toml::Value>, features: Option<&toml::Value>) -> (BTreeMap<String, Dep>, Vec<ConditionalFeature>) {
    let mut dependencies = BTreeMap::new();
    let mut cond = Vec::new();
    if let Some(deps) = deps {
        dependencies = make_deps(base_path, deps)
    }

    if let Some(features) = features {
        for (a, b) in features.as_table().unwrap().iter() {
            // println!("a {:?}, b {:?}", a, b);
            for b in b.as_array().unwrap() {
                if let Some(b) = b.as_str() {
                    let mut b = b.split('/');
                    match (b.next(), b.next()) {
                        (Some(c), Some(d)) => {
                            if let Some(dep) = dependencies.get_mut(c) {
                                debug!("conditional: {:?} {:?} {:?}", c, a, d);
                                dep.conditional_features.push(ConditionalFeature {
                                    feature: a.to_string(),
                                    dep_feature: d.to_string()
                                })
                            }
                        }
                        (Some(c), None) => {
                            // println!("conditional: {:?} {:?} {:?}", c, a, d);
                            cond.push(ConditionalFeature {
                                feature: a.to_string(),
                                dep_feature: c.to_string()
                            })
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    (dependencies, cond)
}
