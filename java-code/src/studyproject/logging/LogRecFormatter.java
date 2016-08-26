package studyproject.logging;

import java.util.logging.Formatter;
import java.util.logging.LogRecord;

import studyproject.API.Core.Utils;

public class LogRecFormatter extends Formatter {

	@Override
	public String format(LogRecord record) {
		return (Utils.formatUnixTimestamp(record.getMillis()) + ": " + record.getMessage());
	}

}
