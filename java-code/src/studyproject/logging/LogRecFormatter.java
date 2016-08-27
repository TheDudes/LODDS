package studyproject.logging;

import java.util.logging.Formatter;
import java.util.logging.LogRecord;

import studyproject.API.Core.Utils;

/**
 * A Formatter which is used to format logRecords to the format "Timestamp:
 * logMessage" where Timestamp is currently formatted with
 * {@link Utils}.formatUnixTimestamp
 * 
 * @author ninti
 *
 */
public class LogRecFormatter extends Formatter {

	@Override
	public String format(LogRecord record) {
		return (Utils.formatUnixTimestamp(record.getMillis()) + ": " + record.getMessage());
	}

}
