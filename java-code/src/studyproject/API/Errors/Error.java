package studyproject.API.Errors;

import java.util.logging.Level;
import java.util.logging.LogRecord;

import studyproject.logging.LogKey;
import studyproject.API.Core.Utils;
import studyproject.logging.APILvl;

/**
 * java.util.logging {@link LogRecord} class which holds enhanced information
 * about a log message like, {@link APILvl} and {@link LogKey}
 * 
 * @author ninti
 *
 */
public class Error extends LogRecord {

	private static final long serialVersionUID = 4997203555037544881L;
	private LogKey logKey;
	private APILvl apiLvl;
	private String msg;
	private String thrownBy;
	private String timestamp;
	private String logLevelString;
	private static final String ERRORMSG = "ERROR";

	/**
	 * @deprecated do not use this constructor
	 * @param level
	 * @param msg
	 */
	public Error(Level level, String msg) {
		super(level, msg);
		this.timestamp = Utils.formatUnixTimestamp(this.getMillis());
		this.logLevelString = getLvlMessage();
	}

	/**
	 * Constructor to create a new instance of an Error LogRecord.
	 * 
	 * @param level
	 *            the LogLevel {@link Level}
	 * @param logKey
	 *            the {@link LogKey}
	 * @param apiLvl
	 *            the {@link APILvl} where the Error is produced
	 * @param msg
	 *            the specific error message which all non gui logger shall log
	 * @param errorMsg
	 *            the single error message returned by
	 *            ErrLog.getErrorMsg(errorCode)
	 */
	public Error(Level level, LogKey logKey, APILvl apiLvl, String thrownBy, String msg, String errorMsg) {
		super(level, msg);
		this.logKey = logKey;
		this.apiLvl = apiLvl;
		this.msg = errorMsg;
		this.thrownBy = thrownBy;
		this.timestamp = Utils.formatUnixTimestamp(this.getMillis());
		this.logLevelString = getLvlMessage();
	}

	/**
	 * Returns an String which represents the Log Level, where Severe gets
	 * returned as "ERROR" and all other Log levels as "INFO"
	 * 
	 * @return a String either "ERROR" or "INFO"
	 */
	public String getLvlMessage() {
		if (this.getLevel() == Level.SEVERE)
			return ERRORMSG;
		return this.getLevel().toString();
	}

	public LogKey getLogKey() {
		return logKey;
	}

	public void setLogKey(LogKey logKey) {
		this.logKey = logKey;
	}

	public APILvl getApiLvl() {
		return apiLvl;
	}

	public void setApiLvl(APILvl apiLvl) {
		this.apiLvl = apiLvl;
	}

	public String getMsg() {
		return msg;
	}

	public void setMsg(String msg) {
		this.msg = msg;
	}

	public String getThrownBy() {
		return thrownBy;
	}

	public void setThrownBy(String thrownBy) {
		this.thrownBy = thrownBy;
	}

	public String getTimestamp() {
		return timestamp;
	}

	public void setTimestamp(String timestamp) {
		this.timestamp = timestamp;
	}

	/**
	 * Returns true if the LogKey is a broadcast specific one (either
	 * LogKey.broadcastReceived or LogKey.broadcastSent)
	 * 
	 * @return true if the LogKey is one of LogKey.broadcastReceived or
	 *         LogKey.broadcastSent
	 */
	public boolean isBroadcast() {
		if (logKey == LogKey.broadcastReceived || logKey == LogKey.broadcastSent)
			return true;
		return false;

	}

	/**
	 * Returns true if LogKey is LogKey.getSent
	 * 
	 * @return true if LogKey is LogKey.getSent
	 */
	public boolean isGetSent() {
		if (logKey == LogKey.getSent)
			return true;
		return false;
	}

	/**
	 * Returns true if LogKey is LogKey.getReceived
	 * 
	 * @return true if LogKey is LogKey.getReceived
	 */
	public boolean isGetRec() {
		if (logKey == LogKey.getReceived)
			return true;
		return false;
	}

	/**
	 * Returns the logLevelString
	 * 
	 * @return the logLevelString
	 */
	public String getLogLevelString() {
		return logLevelString;
	}

}
