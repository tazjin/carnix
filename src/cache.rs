use rusqlite;
use std::path::Path;
use krate::Src;
use serde_json;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct Prefetch {
    pub prefetch: Src,
    pub path: PathBuf,
}

#[derive(Debug)]
pub struct Cache {
    cache: rusqlite::Connection,
}

impl Cache {
    pub fn new<P:AsRef<Path>>(file: P) -> Self {

        let conn = rusqlite::Connection::open(
            file,
        ).unwrap();

        conn.execute("CREATE TABLE IF NOT EXISTS fetches (
                  id              INTEGER PRIMARY KEY,
                  name            TEXT UNIQUE NOT NULL,
                  json            TEXT,
                  path            TEXT NOT NULL
                  )", &[]).unwrap();
        Cache {
            cache: conn,
        }
    }

    pub fn get(&mut self, url: &str) -> Option<Prefetch> {
        let mut get_stmt = self.cache.prepare("SELECT json, path FROM fetches WHERE name = ?1").unwrap();
        let pre: Option<Prefetch> = get_stmt.query_map(&[&url], |row| {
            let pref: String = row.get(0);
            let path: String = row.get(1);
            Prefetch {
                prefetch: serde_json::from_str(&pref).unwrap(),
                path: Path::new(&path).to_path_buf()
            }
        }).unwrap().next().and_then(|x| x.ok());
        pre
    }

    pub fn insert(&mut self, url: &str, prefetch: &Prefetch) {
        let mut insert_stmt = self.cache.prepare("INSERT INTO fetches(name, json, path) VALUES(?1, ?2, ?3)").unwrap();
        insert_stmt.execute(&[
            &url,
            &serde_json::to_string(&prefetch.prefetch).unwrap(),
            &prefetch.path.to_str()
        ]).unwrap();
    }
}
