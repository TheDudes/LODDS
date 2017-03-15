package studyproject.gui.mainWindow.logArea;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import com.sun.javafx.scene.control.skin.TableViewSkin;
import com.sun.javafx.scene.control.skin.VirtualFlow;
import javafx.application.Platform;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Skin;
import javafx.scene.control.TableView;
import studyproject.App;
import studyproject.API.Errors.Error;
import studyproject.logging.LogKey;

public class LogAreaHandler extends Handler {

	private TableView<Error> toLogTo;
	private SimpleBooleanProperty error;
	public static final int MAX_LOG_MSG_COUNT = Integer.valueOf(App.properties.getProperty("MAX_LOG_MSG_COUNT"));
	@SuppressWarnings("rawtypes")
	private VirtualFlow virtualFlow;
	private SimpleBooleanProperty info;
	private SimpleBooleanProperty getRec;
	private SimpleBooleanProperty getSent;
	private SimpleBooleanProperty broadcast;
	private boolean wasRemoved;

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
		getVirtualFlow();

	}

	/**
	 * Extracts the VirtualFlow from the LogAreas skinProperty, the VirtualFlow
	 * is used for scrolling reasons
	 */
	@SuppressWarnings("rawtypes")
	private void getVirtualFlow() {
		toLogTo.skinProperty().addListener(new ChangeListener<Skin>() {
			@SuppressWarnings("unchecked")
			@Override
			public void changed(ObservableValue<? extends Skin> ov, Skin t, Skin t1) {
				if (t1 == null) {
					return;
				}

				TableViewSkin tvs = (TableViewSkin) t1;
				ObservableList<Node> kids = tvs.getChildren();

				if (kids == null || kids.isEmpty()) {
					return;
				}
				virtualFlow = (VirtualFlow) kids.get(1);
			}
		});
	}

	@Override
	public void close() throws SecurityException {

	}

	@Override
	public void flush() {

	}

	/**
	 * Publishes a record if the record is a instance of {@link Error},
	 * isLoggable(which checks appropiate log level) and shouldBeLogged which
	 * checks for the users settings, which messages shall be logged. If the
	 * size of the log messages exceeds MAX_LOG_MSG_COUNT an entry is deleted
	 * before another one is added. If an entry was removed the virtual flow
	 * will scroll up for one entry to keep track of the current focused part of
	 * the messages.
	 *
	 * @param record
	 *            the log record which shall be published
	 */
	@Override
	public void publish(LogRecord record) {
		if (!(record instanceof Error))
			return;
		if (!isLoggable(record))
			return;
		if (shouldBeLogged((Error) record)) {
			wasRemoved = false;
			Platform.runLater(() -> {
				if (toLogTo.getItems().size() >= MAX_LOG_MSG_COUNT) {
					toLogTo.getItems().remove(0);
					wasRemoved = true;
				}
				toLogTo.getItems().add((Error) record);

				if (wasRemoved)
					virtualFlow.scrollToOffset(-1);
			});
		}
	}

	/**
	 * Inspects the passed {@link Error}(the LogRecord), returns True if the log
	 * record is either Level.Info or Severe, and the corresponding check box is
	 * set to true. Furthermore checks if the {@link Error} got {@link LogKey}
	 * broadcast, getRec, getFile and the corresponding checkboxes are set. All
	 * Errors which got other {@link LogKey} are logged to the table view at the
	 * moment.
	 *
	 * @param record
	 *            The LogRecord received by the publish function which is
	 *            instanceof {@link Error}
	 * @return true if the {@link Error} should be logged to the TableView false
	 *         if not
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
		if (getRec.get() == false && record.isRespondFile())
			return false;
		if (getSent.get() == false && record.isGetFile())
			return false;
		return true;
	}

}
