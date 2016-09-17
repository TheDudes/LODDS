package studyproject.logging;

import java.util.logging.Handler;
import java.util.logging.LogRecord;

/**
 * A java.utli.logging.Handler which uses the {@link LogRecFormatter} as
 * Formatter. Currently only uses System.err.println to publish logRecords
 * 
 * @author ninti
 *
 */
public class LogConsoleHandler extends Handler {
	public LogConsoleHandler() {
		super();
		this.setFormatter(new LogRecFormatter());
	}

	@Override
	public void close() throws SecurityException {

	}

	@Override
	public void flush() {

	}

	@Override
	public void publish(LogRecord record) {
		if (!isLoggable(record))
			return;
		System.err.println(getFormatter().format(record));
	}

}
