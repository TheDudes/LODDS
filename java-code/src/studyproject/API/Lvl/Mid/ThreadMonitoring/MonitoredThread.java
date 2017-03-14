package studyproject.API.Lvl.Mid.ThreadMonitoring;

import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.StringProperty;

public interface MonitoredThread {

	/**
	 * Shall return true if the thread is running and false if not
	 * 
	 * @return true if the thread is running and false if not
	 */
	SimpleBooleanProperty isRunning();

	/**
	 * Sets the running boolean of the MonitoredThread
	 */
	void setRunning(boolean toSet);

	/**
	 * Shall return true if the MonitoredThread is finished or false if not
	 * 
	 * @return true if the MonitoredThread is finished or false if not
	 */
	SimpleBooleanProperty isFinished();

	/**
	 * Returns true if the MonitoredThread is in a queue of a
	 * {@link MultipleDownloadHelper}
	 * 
	 * @return true if the MonitoredThread is in a queue of a
	 *         {@link MultipleDownloadHelper}
	 */
	boolean isOneOfMultiple();

	/**
	 * Sets the boolean oneOfMultiple which states if the
	 * {@link MonitoredThread} is in a queue of a {@link MultipleDownloadHelper}
	 */
	void setOneOfMultiple(boolean oneOfMultiple);

	/**
	 * Returns the current progress of the monitored thread ranges from 0.0 for
	 * 0% progress to 1.0 for 100% progress
	 * 
	 * @return the current progress of the monitored thread ranges from 0.0 for
	 *         0% progress to 1.0 for 100% progress
	 */
	SimpleDoubleProperty getProgress();

	/**
	 * Shall return a long which states the size in bytes which where already
	 * sent or received
	 * 
	 * @return a long which states the size in bytes which where already sent or
	 *         received
	 */
	SimpleLongProperty getDoneSize();

	/**
	 * Returns the whole size in bytes which needs to be either received or sent
	 * as a long
	 * 
	 * @return whole size in bytes which needs to be either received or sent as
	 *         a long
	 */
	long getWholeSize();

	/**
	 * Returns the name of the currently downloaded file
	 * 
	 * @return the name of the currently downloaded file
	 */
	StringProperty getCurrentFileName();

	/**
	 * Returns the String which shall be displayed on the downloads widget, for
	 * a multiple download this should be the base/topdir and for a single file
	 * download this should be the filename
	 * 
	 * @return the String which shall be displayed on the downloads widget
	 */
	String getNameToDisplay();

}
