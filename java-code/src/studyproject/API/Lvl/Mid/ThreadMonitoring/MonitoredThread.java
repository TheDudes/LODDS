package studyproject.API.Lvl.Mid.ThreadMonitoring;

import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.StringProperty;

public interface MonitoredThread {

	/**
	 * Returns true if the MonitoredThread is in a queue of a
	 * {@link MultipleDownloadHelper}
	 * 
	 * @return
	 */
	boolean isOneOfMultiple();

	/**
	 * Sets the boolean oneOfMultiple which states if the
	 * {@link MonitoredThread} is in a quere of a {@link MultipleDownloadHelper}
	 */
	void setOneOfMultiple(boolean oneOfMultiple);

	/**
	 * Returns the submitted boolean of the Monitored thread, which states if
	 * the thread was submitted to the {@link ThreadExecutor}
	 * 
	 * @return true if the Monitored thread was submitted to the
	 *         {@link ThreadExecutor} and false if not
	 */
	boolean isSubmitted();

	/**
	 * Function to set the submitted boolean of the @{@link MonitoredThread},
	 * which is done by the @{@link ThreadExecutor}. This function is called for
	 * every Thread submitted to the {@link ThreadExecutor}
	 * 
	 * @param toSet
	 *            the bool to set, either true, if the thread was submitted to
	 *            the {@link ThreadExecutor} or false if not
	 */
	void setSubmitted(boolean toSet);

	/**
	 * 0.00 to 1.00 for finished
	 * 
	 * @param toSet
	 *            the number to set
	 */
	void setProgress(SimpleDoubleProperty toSet);

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
	 * @return
	 */
	SimpleLongProperty getDoneSize();

	/**
	 * Returns the whole size in bytes which needs to be either received or sent
	 * 
	 * @return
	 */
	long getWholeSize();

	/**
	 * Returns the name of the currently downloaded file
	 * 
	 * @return
	 */
	StringProperty getCurrentFileName();

	/**
	 * Returns the String which shall be displayed on the downloads widget, for
	 * a multiple download this should be the top dir and for a single file
	 * download this should be the filename
	 * 
	 * @return
	 */
	String getNameToDisplay();

}
