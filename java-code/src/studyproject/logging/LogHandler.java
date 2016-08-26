package studyproject.logging;

import java.util.logging.Handler;
import java.util.logging.LogRecord;

public class LogHandler extends Handler{
	public LogHandler() {
		super();
		this.setFormatter(new LogRecFormatter());
	}
	
	@Override
	public void close() throws SecurityException {
		
	}

	@Override
	public void flush() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void publish(LogRecord record) {
		if (!isLoggable(record))
			return;
		System.err.println(getFormatter().format(record));
	}

}
