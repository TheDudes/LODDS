package studyproject.API.Errors;

import java.util.logging.Level;
import java.util.logging.Logger;

import studyproject.logging.LogKey;
import studyproject.logging.APILvl;

/**
 * Class to Handle Error codes and log them to the in the constructor overgiven
 * {@link Logger}
 * 
 * @author ninti
 *
 */
public class ErrorHandler {

	private Logger logger;

	/**
	 * 
	 * @param logger
	 *            which shall log the logRecords
	 */
	public ErrorHandler(Logger logger) {
		this.logger = logger;
	}

	/**
	 * Creates a {@link Error} logs it to the in the constructor overgiven
	 * {@link Logger} and returns the Object, will format the logRecord to
	 * '{@link LogKey}: {@link APILvl} : Thrown By 'functionName': ErrorMessage'
	 * 
	 * @param level
	 *            the LogLevel
	 * @param logKey
	 *            the LogKey
	 * @param apiLvl
	 *            the ApiLvl where the error was thrown
	 * @param errorType
	 *            The errorType to get the Error Message
	 * @param functionName
	 *            the function which have thrown the Error
	 * @return the Error Object with filled information
	 */
	public Error logError(Level level, LogKey logKey, APILvl apiLvl, ErrorTypes errorType, String functionName) {
		Error error = new Error(level, logKey, apiLvl, getErrorMsg(errorType, logKey, apiLvl, functionName));
		logger.log(error);
		return error;
	}

	/**
	 * Uses the errorType to determine the ErrorMsg and format the determined
	 * ErrorMsgString and errorMsgPrefix to 'errorMsgPrefix: errorMessage'
	 * 
	 * @param errorType
	 *            The Type Of ErrorTypes to determine the error message
	 * @param logKey
	 *            the certein {@link LogKey}
	 * @param apiLvl
	 *            the {@link APILvl} where the error was produced
	 * @param functionName
	 *            the function which have thrown the error
	 * @return a string formatted like '{@link LogKey}: {@link APILvl} :Thrown
	 *         By 'functionName': ErrorMessage'
	 */
	private static String getErrorMsg(ErrorTypes errorType, LogKey logKey, APILvl apiLvl, String functionName) {
		String errorMsg = "";
		switch (errorType) {
		case noError:
			errorMsg = "Tried to log no error";
			break;
		case bufferedInputStream:
			errorMsg = "Error occured while reading from " + errorType.toString();
			break;
		case bufferedReader:
			errorMsg = "Error occured while reading from " + errorType.toString();
			break;
		case denied:
			errorMsg = "Other user denied the Request";
			break;
		case fileInputStream:
			errorMsg = "Error occured while reading from " + errorType.toString();
			break;
		case fileOutputStream:
			errorMsg = "Error occured while writing to " + errorType.toString();
			break;
		case indexOutOfBound:
			errorMsg = errorType.toString();
			break;
		case noSuchAlgorithm:
			errorMsg = errorType.toString();
			break;
		case numberFormat:
			errorMsg = "NumberFormatException";
			break;
		case socket:
			errorMsg = "SocketError";
			break;
		case timeout:
			errorMsg = "Timeout";
			break;
		

		}
		return getErrorMsgPrefix(logKey, apiLvl, functionName) + ": " + errorMsg;

	}

	/**
	 * 
	 * @param logKey
	 *            the certein {@link LogKey}
	 * @param apiLvl
	 *            the {@link APILvl} where the error was produced
	 * @param functionName
	 *            the function which have thrown the error
	 * @return a string formatted like '{@link LogKey}: {@link APILvl} :Thrown
	 *         By 'functionName'
	 */
	private static String getErrorMsgPrefix(LogKey logKey, APILvl apiLvl, String functionName) {
		return logKey.toString() + ": " + apiLvl.toString() + ": Thrown By '" + functionName + "'";
	}

}
