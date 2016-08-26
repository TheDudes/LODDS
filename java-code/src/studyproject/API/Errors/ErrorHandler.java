package studyproject.API.Errors;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;


import studyproject.logging.LogKey;
import studyproject.logging.LogRecFormatter;
import studyproject.logging.APILvl;

public class ErrorHandler {

	private static Logger logger = Logger.getGlobal();

	public static void init() {

		for (Handler handler : logger.getHandlers()) {
			handler.setFormatter(new LogRecFormatter());
		}
	}

	public static Error logError(Level level, LogKey logKey, APILvl apiLvl, ErrorTypes errorType, String functionName) {
		Error error = new Error(level, logKey, apiLvl, getErrorMsg(errorType, getErrorMsgPrefix(logKey, apiLvl)));
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

	private static String getErrorMsgPrefix(LogKey logKey, APILvl apiLvl) {
		return logKey.toString() + ": " + apiLvl.toString();
	}
}
