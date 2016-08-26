package studyproject.API.Errors;

import java.util.logging.Level;
import java.util.logging.LogRecord;

import studyproject.logging.LogKey;
import studyproject.logging.APILvl;

public class Error extends LogRecord {

	private static final long serialVersionUID = 4997203555037544881L;
	private LogKey logKey;
	private APILvl apiLvl;

	public Error(Level level, String msg) {
		super(level, msg);
	}

	public Error(Level level, LogKey logKey, APILvl apiLvl, String msg) {
		super(level, msg);
		this.logKey = logKey;
		this.apiLvl = apiLvl;
	}

	public LogKey getLogKey() {
		return logKey;
	}

	public void setLogKey(LogKey logKey) {
		this.logKey = logKey;
	}

	public APILvl getLogTag() {
		return apiLvl;
	}

	public void setLogTag(APILvl logTag) {
		this.apiLvl = logTag;
	}

}
