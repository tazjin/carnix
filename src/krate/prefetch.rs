use super::*;
use std::io::Read;
use toml::Value;
use std::path::PathBuf;

impl Crate {

    pub fn prefetch(&self, cache: &mut Cache, source_type: &SourceType) -> Result<Meta, Error> {
        let mut cargo_toml_path = PathBuf::new();
        let prefetch = match *source_type {
            SourceType::Path { ref path, ref workspace_member } => {
                debug!("path: {:?}", path);
                cargo_toml_path.push(path);
                if let Some(ref mem) = *workspace_member {
                    cargo_toml_path.push(mem);
                }
                Prefetch {
                    path: path.to_path_buf(),
                    prefetch: Src::Path {
                        path: path.to_path_buf(),
                        workspace_member: workspace_member.as_ref().map(|x| x.to_path_buf()),
                    }
                }
            }
            SourceType::CratesIO => {
                let prefetch = self.prefetch_path(cache)?;
                cargo_toml_path.push(&prefetch.path);
                prefetch
            },
            SourceType::Git { ref url, ref rev } => {
                let prefetch = self.prefetch_git(url, rev, cache)?;
                cargo_toml_path.push(&prefetch.path);
                prefetch
            },
            _ => panic!("unsupported source")
        };

        debug!("src = {:?}", prefetch.prefetch);
        cargo_toml_path.push("Cargo.toml");
        debug!("cargo_toml: {:?}", cargo_toml_path);

        let mut f = std::fs::File::open(&cargo_toml_path)?;
        let mut toml = String::new();
        f.read_to_string(&mut toml).unwrap();
        let mut v:toml::Value = match toml::de::from_str(&toml) {
            Ok(v) => v,
            Err(e) => {
                error!("{:?}: {:?}", cargo_toml_path, e);
                return Err(e.into())
            }
        };
        let v = v.as_table_mut().expect("v not a table");


        let (dependencies, implied) = make_dependencies(&prefetch.path, v.get("dependencies"), v.get("features"));
        let (build_dependencies, _) = make_dependencies(&prefetch.path, v.get("build-dependencies"), None);
        debug!("dependencies of {:?} = {:?}", self.name, dependencies);

        let mut target_dependencies = Vec::new();
        if let Some(target) = v.remove("target") {
            let target = if let Value::Table(target) = target {
                target
            } else {
                panic!("target not a table")
            };
            debug!("target = {:?}", target);
            for (a, b) in target {
                debug!("a = {:?}", a);
                let (dependencies, _) = make_dependencies(&prefetch.path, b.get("dependencies"), None);
                target_dependencies.push((a, dependencies))
            }
            debug!("target_deps {:?}", target_dependencies);
        }

        // Grab the authors from Cargo.toml, so we can create the
        // CARGO_PKG_AUTHORS environment variable at build time.
        let authors = v.get("package")
            .and_then(|x| x.get("authors"))
            .and_then(|x| x.as_array())
            .map(|x| {
                x.iter()
                    .map(|y| y.as_str().unwrap().to_owned())
                    .collect::<Vec<_>>()
            })
            .unwrap_or_else(Vec::new);

        let (default_features, declared_features) = features(v);
        let include = if let Some(inc) = v.get("package").unwrap().as_table().unwrap().get("include") {
            Some(inc.as_array().unwrap().into_iter().map(|x| x.as_str().unwrap().to_string()).collect())
        } else {
            None
        };
        Ok(Meta {
            src: prefetch.prefetch,
            include,
            dependencies,
            declared_dependencies: declared_dependencies(v),
            target_dependencies,
            build_dependencies,
            crate_file: crate_file(v),
            lib_name: lib_name(v),
            proc_macro: is_proc_macro(v),
            plugin: is_plugin(v),
            crate_type: crate_type(v),
            default_features,
            declared_features,
            use_default_features: None,
            features: BTreeSet::new(),
            build: build(v),
            implied_features: implied,
            bins: bins(v),
            authors,
        })
    }
}

fn crate_file(v: &BTreeMap<String, toml::Value>) -> String {
    if let Some(crate_file) = v.get("lib") {
        let crate_file = crate_file.as_table().unwrap();
        if let Some(lib_path) = crate_file.get("path") {
            lib_path.as_str().unwrap().to_string()
        } else {
            String::new()
        }
    } else {
        String::new()
    }
}

fn crate_type(v: &BTreeMap<String, toml::Value>) -> Option<String> {
    if let Some(crate_file) = v.get("lib") {
        if let Some(crate_file) = crate_file.as_table() {
            if let Some(crate_type) = crate_file.get("crate-type") {
                return Some(crate_type.as_str().unwrap().to_string())
            }
        }
    }
    None
}

fn lib_name(v: &BTreeMap<String, toml::Value>) -> String {
    if let Some(crate_file) = v.get("lib") {
        if let Some(name) = crate_file.get("name") {
            name.as_str().unwrap().to_string()
        } else {
            String::new()
        }
    } else {
        String::new()
    }
}


fn bins(v: &mut BTreeMap<String, toml::Value>) -> Vec<Bin> {
    if let Some(toml::Value::Array(bins)) = v.remove("bin") {
        bins.into_iter().map(|mut x| {
            let bin = x.as_table_mut().unwrap();
            Bin {
                name: if let Some(toml::Value::String(s)) = bin.remove("name") { Some(s) } else { None },
                path: if let Some(toml::Value::String(s)) = bin.remove("path") { Some(s) } else { None },
            }
        }).collect()
    } else {
        Vec::new()
    }
}

fn is_proc_macro(v: &BTreeMap<String, toml::Value>) -> bool {
    if let Some(crate_file) = v.get("lib") {
        if let Some(&toml::Value::Boolean(proc_macro)) = crate_file.get("proc-macro") {
            proc_macro
        } else {
            false
        }
    } else {
        false
    }
}

fn is_plugin(v: &BTreeMap<String, toml::Value>) -> bool {
    if let Some(crate_file) = v.get("lib") {
        if let Some(&toml::Value::Boolean(plugin)) = crate_file.get("plugin") {
            plugin
        } else {
            false
        }
    } else {
        false
    }
}

fn build(v: &BTreeMap<String, toml::Value>) -> String {
    let package = v.get("package").unwrap();
    if let Some(build) = package.as_table().unwrap().get("build") {
        build.as_str().unwrap().to_string()
    } else {
        String::new()
    }
}

fn features(v: &BTreeMap<String, toml::Value>) -> (Vec<String>, BTreeSet<String>) {
    let mut default_features = Vec::new();
    let mut declared_features = BTreeSet::new();

    if let Some(features) = v.get("features") {
        let features = features.as_table().unwrap();
        if let Some(default) = features.get("default") {
            default_features.extend(
                default.as_array().unwrap().into_iter().map(|x| x.as_str().unwrap().to_string())
            )
        }
        for (f, _) in features.iter() {
            if f != "default" {
                declared_features.insert(f.to_string());
            }
        }
    }
    (default_features, declared_features)
}

fn declared_dependencies(v: &BTreeMap<String, toml::Value>) -> BTreeSet<String> {
    let mut declared_dependencies = BTreeSet::new();
    if let Some(deps) = v.get("dependencies") {
        if let Some(deps) = deps.as_table() {
            for (f, _) in deps.iter() {
                declared_dependencies.insert(f.clone());
            }
        }
    }
    if let Some(deps) = v.get("dev-dependencies") {
        if let Some(deps) = deps.as_table() {
            for (f, _) in deps.iter() {
                declared_dependencies.insert(f.clone());
            }
        }
    }
    declared_dependencies
}
