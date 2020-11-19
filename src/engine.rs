use std::collections::HashMap;
use tokio::sync::RwLock;

use crate::resp::RespValue;
use crate::error::EngineError;


#[derive(Debug)]
pub(crate) struct Cache {
    store: RwLock<HashMap<String, RespValue>>,
}

impl Cache {
    pub(crate) fn new() -> Self {
        Self {
            store: RwLock::new(HashMap::new()),
        }
    }

    pub(crate) async fn get(&self, key: &str) -> Result<RespValue, EngineError> {
        let readonly_store = self.store.read().await;

        Ok(match readonly_store.get(key) {
            Some(value) => {
                if value.is_simple_string() || value.is_bulk_string() {
                    value.clone()
                } else {
                    RespValue::error("Value isn't a string")
                }
            }
            None => RespValue::Null,
        })
    }

    pub(crate) async fn get_str(&self, key: &str) -> Result<RespValue, EngineError> {
        let value = self.get(key).await?;

        if value.is_simple_string() || value.is_bulk_string() {
            Ok(value)
        } else {
            Ok(RespValue::error("Value isn't a string"))
        }
    }

    pub(crate) async fn set(&self, key: &str, value: RespValue) -> Result<RespValue, EngineError> {
        eprintln!("SETTING '{}' to  '{:?}'", key, value);
        let mut store = self.store.write().await;
        store.insert(key.to_string(), value);
        Ok(RespValue::simple_string("OK"))
    }

    pub(crate) async fn set_str(&self, key: &str, value:RespValue) -> Result<RespValue, EngineError> {
        if value.is_simple_string() || value.is_bulk_string() {
            Ok(self.set(key, value).await?)
        } else {
            Ok(RespValue::error("Value isn't a string"))
        }
    }

}

