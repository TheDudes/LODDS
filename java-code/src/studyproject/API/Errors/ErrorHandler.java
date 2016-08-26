package studyproject.API.Errors;

import java.util.logging.Level;
import java.util.logging.Logger;

import studyproject.logging.LogKey;
import studyproject.logging.APILvl;

public class ErrorHandler {

	private Logger logger;

	public ErrorHandler(Logger logger) {
		this.logger = logger;
	}

	public Error logError(Level level, LogKey logKey, APILvl apiLvl, ErrorTypes errorType, String functionName) {
		Error error = new Error(level, logKey, apiLvl,
				getErrorMsg(errorType, getErrorMsgPrefix(logKey, apiLvl, functionName)));
		logger.log(error);
		return error;
	}

	private static String getErrorMsg(ErrorTypes errorType, String errorMsgPrefix) {
		String errorMsg = "";
		switch (errorType) {
		case IO:
			errorMsg = "IO FAIL";
			break;
		case blah:
			break;
		case blub:
			break;
		default:
			break;
		}
		return errorMsgPrefix + ": " + errorMsg;

	}

	private static String getErrorMsgPrefix(LogKey logKey, APILvl apiLvl, String functionName) {
		return logKey.toString() + ": " + apiLvl.toString() + ": Thrown By '" + functionName + "'";
	}


}
