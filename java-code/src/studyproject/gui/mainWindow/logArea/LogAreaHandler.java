package studyproject.gui.mainWindow.logArea;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;


import javafx.scene.control.TableView;
import studyproject.API.Errors.Error;
import studyproject.logging.LogRecFormatter;

public class LogAreaHandler extends Handler {


	private TableView<Error> toLogTo;

	public LogAreaHandler(Level level, TableView<Error> toLogTo) {
		super();
		this.setFormatter(new LogRecFormatter());
		this.setLevel(level);
		this.toLogTo = toLogTo;

	}

	@Override
	public void close() throws SecurityException {
		// TODO Auto-generated method stub

	}

	@Override
	public void flush() {
		// TODO Auto-generated method stub

	}

	@Override
	public void publish(LogRecord record) {
		if (!(record instanceof Error))
			return;
		if (!isLoggable(record))
			return;
		toLogTo.getItems().add((Error) record);
	}

}
