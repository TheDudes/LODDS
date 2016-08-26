package studyproject.logging;

import java.util.logging.Formatter;
import java.util.logging.LogRecord;

import studyproject.API.Core.Utils;

public class LogRecFormatter extends Formatter {
	private StringBuilder sb;

	@Override
	public String format(LogRecord record) {
		sb = new StringBuilder();
		//append formatted unix timestamp
		sb.append(Utils.formatUnixTimestamp(record.getMillis()));
		sb.append(": ");
		sb.append(record.getMessage());
		return sb.toString();
	}

}
