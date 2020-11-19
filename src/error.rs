use std::error::Error;

#[derive(Debug)]
pub(crate) enum EngineError {
    #[allow(dead_code)]
    Unknown {
        message: String,
    },

    IOError {
        cause: std::io::Error,
    },
    JoinError {
        cause: tokio::task::JoinError,
    },
}

impl EngineError {
    pub(crate) fn unknown(message: &str) -> Self {
        Self::Unknown {
            message: message.to_string(),
        }
    }
}

impl std::fmt::Display for EngineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EngineError::Unknown { ref message, .. } => {
                write!(f, "EngineError {{ Unknown {} }}", message)
            }

            EngineError::IOError { ref cause, .. } => {
                write!(f, "EngineError {{ IOError {} }}", cause)
            }

            EngineError::JoinError { ref cause, .. } => {
                write!(f, "EngineError {{ JoinError {} }}", cause)
            }
        }
    }
}

impl Error for EngineError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            EngineError::Unknown { .. } => None,
            EngineError::IOError { ref cause, .. } => Some(cause),
            EngineError::JoinError { ref cause, .. } => Some(cause),
        }
    }
}

impl From<std::io::Error> for EngineError {
    fn from(cause: std::io::Error) -> Self {
        EngineError::IOError { cause }
    }
}

impl From<tokio::task::JoinError> for EngineError {
    fn from(cause: tokio::task::JoinError) -> Self {
        EngineError::JoinError { cause }
    }
}
