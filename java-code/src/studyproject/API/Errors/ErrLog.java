package studyproject.API.Errors;

import java.util.logging.Level;
import java.util.logging.Logger;

import studyproject.logging.LogKey;
import studyproject.logging.APILvl;

/**
 * Class to Handle Error codes and log them to the in the constructor passed
 * {@link Logger}
 * 
 * @author ninti
 *
 */
public class ErrLog {

	/**
	 * Creates a {@link Error} logs it to the global logger and returns the
	 * Object, will format the error message to '{@link LogKey}: {@link APILvl}
	 * : Thrown By 'functionName': ErrorMessage'
	 * 
	 * @param level
	 *            the LogLevel
	 * @param logKey
	 *            the LogKey
	 * @param apiLvl
	 *            the ApiLvl where the error was thrown
	 * @param errorCode
	 *            the ordinal number of the {@link ErrorTypes}
	 * @param functionName
	 *            the function which have thrown the Error
	 * @return the Error Object with filled information
	 */
	public static Error log(Level level, LogKey logKey, APILvl apiLvl, int errorCode, String functionName) {
		Error error = getError(level, logKey, apiLvl, errorCode, getErrorMsg(errorCode, logKey, apiLvl, functionName));
		Logger.getGlobal().log(error);
		return error;
	}

	/**
	 * Creates a {@link Error} and returns the Object, will format the error
	 * message to '{@link LogKey}: {@link APILvl} : Thrown By 'functionName':
	 * ErrorMessage'
	 * 
	 * @param level
	 *            the LogLevel
	 * @param logKey
	 *            the LogKey
	 * @param apiLvl
	 *            the ApiLvl where the error was thrown
	 * @param errorCode
	 *            the ordinal number of the {@link ErrorTypes}
	 * @param functionName
	 *            the function which have thrown the Error
	 * @return the Error Object with filled information
	 */
	public static Error getError(Level level, LogKey logKey, APILvl apiLvl, int errorCode, String functionName) {
		return new Error(level, logKey, apiLvl, getErrorMsg(errorCode, logKey, apiLvl, functionName));
	}

	/**
	 * Uses the errorCode to determine the ErrorMsg and format the determined
	 * ErrorMsgString and errorMsgPrefix to 'errorMsgPrefix: errorMessage'
	 * 
	 * @param errorCode
	 *            the ordinal number of the {@link ErrorTypes}
	 * @param logKey
	 *            the certein {@link LogKey}
	 * @param apiLvl
	 *            the {@link APILvl} where the error was produced
	 * @param functionName
	 *            the function which have thrown the error
	 * @return a string formatted like "{@link LogKey}: {@link APILvl} :Thrown
	 *         By 'functionName': ErrorMessage"
	 * 
	 */
	public static String getErrorMsg(int errorCode, LogKey logKey, APILvl apiLvl, String functionName) {
		String errorMsg = "No error message found!";
		ErrorTypes errorType = ErrorTypes.valueOf(errorCode);
		switch (errorType) {
		case oK:
			errorMsg = "oK";
			break;
		case connectionClosed:
			errorMsg = "Connection to client closed unexpectedly";
			break;
		case fileNotFound: 
			errorMsg = "The specified File(via Checksum or Pathname) could not be found";
			break;
		case malformedData:
			errorMsg = "Data could not be parsed, since it's not conforment to the specification LODDS";
			break;
		case timoutReached:
			errorMsg = "Timeout has been reached whithout a response";
			break;
		case connectionDenied:
			errorMsg = "Other client denied the conncetion";
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
		StringBuilder sb = new StringBuilder();
		sb.append(logKey.toString());
		sb.append(": ");
		sb.append(apiLvl.toString());
		sb.append(": Thrown By '");
		sb.append(functionName);
		sb.append("'");
		return sb.toString();
	}

}
