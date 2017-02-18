package studyproject.API.Errors;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.logging.Level;
import java.util.logging.Logger;

import studyproject.logging.LogKey;

/**
 * Class to Handle Error codes and log them to the in the constructor passed
 * {@link Logger}
 * 
 * @author ninti
 *
 */
public class ErrorFactory {

	/**
	 * Creates an {@link Error} and returns the Object, will format the error
	 * message to '{@link LogKey}: ErrorMessage'
	 * 
	 * @param level
	 *            the LogLevel
	 * @param logKey
	 *            the LogKey
	 * @param customMsg
	 *            the custom message to display
	 * 
	 * @return the Error Object with filled information
	 */
	public static Error build(Level level, LogKey logKey, String customMsg) {
		return new Error(level, logKey, logKey.toString() + ": " + customMsg, customMsg);
	}

	/**
	 * Creates an {@link Error} and returns the Object, will format the error
	 * message to '{@link LogKey}: ErrorMessage' and will append the printed
	 * stacktrace from the given exception to the ErrorMessage
	 *
	 * @param level
	 *            the LogLevel
	 * @param logKey
	 *            the LogKey
	 * @param customMsg
	 *            the custom message to display
	 * @param exception
	 *            the exception to get the stacktrace from
	 *
	 * @return the Error Object with filled information
	 */
	public static Error build(Level level, LogKey logKey, String customMsg, Exception exception) {
		return new Error(level, logKey, logKey.toString() + ": " + customMsg + "\n" + getStacktraceString(exception),
				customMsg);
	}

	public static Error build(Level level, LogKey logKey, Exception exception) {
		return new Error(level, logKey, logKey.toString() + ": " + getStacktraceString(exception),
				logKey.toString() + ": " + getStacktraceString(exception));
	}

	/**
	 * Creates an {@link Error} and returns the Object, will format the error
	 * message to '{@link LogKey}: ErrorMessage'
	 * 
	 * @param level
	 *            the LogLevel
	 * @param logKey
	 *            the LogKey
	 * @param errorCode
	 *            the ordinal number of the {@link ErrorTypes}
	 * 
	 * @return the Error Object with filled information
	 */
	public static Error build(Level level, LogKey logKey, int errorCode, Exception exception) {
		return new Error(level, logKey,
				logKey.toString() + ": " + getErrorMsg(errorCode) + "\n" + getStacktraceString(exception),
				getErrorMsg(errorCode));
	}

	/**
	 * Creates an {@link Error} and returns the Object, will format the error
	 * message to '{@link LogKey}: ErrorMessage'
	 *
	 * @param level
	 *            the LogLevel
	 * @param logKey
	 *            the LogKey
	 * @param errorCode
	 *            the ordinal number of the {@link ErrorTypes}
	 *
	 * @return the Error Object with filled information
	 */
	public static Error build(Level level, LogKey logKey, int errorCode) {
		return new Error(level, logKey, logKey.toString() + ": " + getErrorMsg(errorCode), getErrorMsg(errorCode));
	}

	/**
	 * Uses the errorCode to determine the ErrorMsg and format the determined
	 * ErrorMsgString and errorMsgPrefix to 'errorMsgPrefix: errorMessage'
	 * 
	 * @param errorCode
	 *            the ordinal number of the {@link ErrorTypes}
	 */
	public static String getErrorMsg(int errorCode) {
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
		case timeoutReached:
			errorMsg = "Timeout has been reached whithout a response";
			break;
		case connectionDenied:
			errorMsg = "Other client denied the conncetion";
			break;

		}
		return errorMsg;

	}

	/**
	 * Extracts the String from the stacktrace with the help of a StringWriter
	 * and returns it
	 * 
	 * @param exception
	 *            The Exception to extract the String from
	 * @return the extracted Stacktrace string from the overgiven exception
	 */
	public static String getStacktraceString(Exception exception) {
		StringWriter errors = new StringWriter();
		exception.printStackTrace(new PrintWriter(errors));
		return errors.toString();
	}

}
