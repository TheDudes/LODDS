package studyproject.API.Errors;

import java.util.logging.Level;
import java.util.logging.LogRecord;

import studyproject.logging.LogKey;
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

	/**
	 * @deprecated do not use this constructor
	 * @param level
	 * @param msg
	 */
	public Error(Level level, String msg) {
		super(level, msg);
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
	 *            the specific error message
	 */
	public Error(Level level, LogKey logKey, APILvl apiLvl,String thrownBy, String msg) {
		super(level, msg);
		this.logKey = logKey;
		this.apiLvl = apiLvl;
		this.msg = msg;
		this.thrownBy = thrownBy;
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

	
	
}
