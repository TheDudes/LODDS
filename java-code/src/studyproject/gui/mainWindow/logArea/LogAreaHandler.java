package studyproject.gui.mainWindow.logArea;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import javafx.beans.property.SimpleBooleanProperty;
import javafx.scene.control.TableView;
import studyproject.API.Errors.Error;
import studyproject.logging.LogKey;

public class LogAreaHandler extends Handler {

	private TableView<Error> toLogTo;
	private SimpleBooleanProperty error;

	private SimpleBooleanProperty info;
	private SimpleBooleanProperty getRec;
	private SimpleBooleanProperty getSent;
	private SimpleBooleanProperty broadcast;

	/**
	 * @deprecated do not use this constructor!
	 */
	public LogAreaHandler() {
		super();
	}

	/**
	 * Creates a new instance of an LogAreaHandler
	 * 
	 * @param level
	 *            the LogLevel which the Handler shall take care of
	 * @param toLogTo
	 *            the TableView where the Error Shall be logged, needs to be an
	 *            TableView<Error>
	 * @param logAreaModel
	 *            the model which holds the Boolean Properties of the checkboxes
	 */
	public LogAreaHandler(Level level, TableView<Error> toLogTo, LogAreaModel logAreaModel) {
		super();
		this.setLevel(level);
		this.toLogTo = toLogTo;
		this.error = logAreaModel.getError();
		this.info = logAreaModel.getInfo();
		this.getRec = logAreaModel.getGetRec();
		this.getSent = logAreaModel.getGetSent();
		this.broadcast = logAreaModel.getBroadcast();
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
		if (shouldBeLogged((Error) record))
			toLogTo.getItems().add((Error) record);
	}

	/**
	 * Inspects the passed {@link Error}(the LogRecord), returns True if the log
	 * record is either Level.Info or Severe, and the corresponding check box is
	 * set to true. Furthermore checks if the {@link Error} got {@link LogKey} broadcast,
	 * getRec, getSent and the corresponding checkboxes are set. All Errors
	 * which got other {@link LogKey} are logged to the table view at the moment.
	 * 
	 * @param record
	 *            The LogRecord received by the publish function which is
	 *            instanceof {@link Error}
	 * @return true if the {@link Error} should be logged to the TableView
	 *         false if not
	 */
	private boolean shouldBeLogged(Error record) {
		Level recordLevel = record.getLevel();
		if (recordLevel != Level.INFO && recordLevel != Level.SEVERE)
			return false;
		if (recordLevel == Level.INFO && info.get() == false)
			return false;
		if (recordLevel == Level.SEVERE && error.get() == false)
			return false;
		if (broadcast.get() == false && record.isBroadcast())
			return false;
		if (getRec.get() == false && record.isGetRec())
			return false;
		if (getSent.get() == false && record.isGetSent())
			return false;
		return true;
	}

}
