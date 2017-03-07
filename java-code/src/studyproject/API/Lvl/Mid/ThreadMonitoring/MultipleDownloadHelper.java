package studyproject.API.Lvl.Mid.ThreadMonitoring;

import javafx.application.Platform;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleStringProperty;
import studyproject.API.Errors.ErrorFactory;
import studyproject.API.Lvl.Mid.Core.FileCoreInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.API.Lvl.Mid.FileConnectionThread;
import studyproject.logging.LogKey;

import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author ninti
 */
public class MultipleDownloadHelper extends Thread implements MonitoredThread {
	Vector<FileCoreInfo> assignedDownloads;
	UserInfo user;
	SimpleDoubleProperty progress = new SimpleDoubleProperty(0);
	boolean submitted = false;
	long wholeSize = 0;
	SimpleLongProperty doneSize = new SimpleLongProperty(0);
	SimpleStringProperty currentDownloadedFile = new SimpleStringProperty("");
	String pathToDownloadTo = "";
	boolean oneOfMultiple = false;
	String baseDir = "";
	Logger logger = Logger.getGlobal();
	SimpleBooleanProperty finished = new SimpleBooleanProperty(false);
	SimpleBooleanProperty running = new SimpleBooleanProperty(true);
	FileConnectionThread currentFileConnectionThread = null;

	public MultipleDownloadHelper(Vector<FileCoreInfo> assignedDownloads, UserInfo user, String pathToDownloadTo,
			String baseDir) {
		this.assignedDownloads = assignedDownloads;
		this.user = user;
		this.baseDir = baseDir;
		this.pathToDownloadTo = pathToDownloadTo;
		determineWholeFileSize();
	}

	private void determineWholeFileSize() {
		for (FileCoreInfo info : assignedDownloads) {
			wholeSize += info.getFilesize();
		}
	}

	@Override
	public void run() {
		int i = 0;
		while (running.get() == true && i < assignedDownloads.size()) {
			FileCoreInfo currentFile = assignedDownloads.get(i);
			Platform.runLater(() -> currentDownloadedFile.set(currentFile.getFileName()));
			currentFileConnectionThread = new FileConnectionThread(user, currentFile.getChecksum(),
					currentFile.getFilesize(), pathToDownloadTo + currentFile.getFilePath());
			currentFileConnectionThread.setOneOfMultiple(true);
			currentFileConnectionThread.getDoneSize().addListener((observable, oldValue, newValue) -> {
				Platform.runLater(() -> {
					doneSize.setValue(doneSize.get() + (newValue.longValue() - oldValue.longValue()));
					progress.set((double) doneSize.get() / (double) wholeSize);
				});
			});
			currentFileConnectionThread.start();
			try {
				currentFileConnectionThread.join();
				i++;
			} catch (InterruptedException e) {
				logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, e));
			}
		}
		finished.setValue(true);
	}

	@Override
	public synchronized SimpleStringProperty getCurrentFileName() {
		return currentDownloadedFile;
	}

	@Override
	public synchronized SimpleLongProperty getDoneSize() {
		return doneSize;
	}

	@Override
	public synchronized long getWholeSize() {
		return wholeSize;
	}

	@Override
	public synchronized boolean isSubmitted() {
		return submitted;
	}

	@Override
	public synchronized void setSubmitted(boolean toSet) {
		this.submitted = toSet;
	}

	@Override
	public synchronized void setProgress(SimpleDoubleProperty toSet) {
		this.progress = toSet;
	}

	@Override
	public synchronized SimpleDoubleProperty getProgress() {
		return progress;
	}

	@Override
	public boolean isOneOfMultiple() {
		return oneOfMultiple;
	}

	@Override
	public void setOneOfMultiple(boolean oneOfMultiple) {
		this.oneOfMultiple = oneOfMultiple;
	}

	@Override
	public SimpleBooleanProperty isRunning() {
		return running;
	}

	@Override
	public void setRunning(boolean toSet) {
		if (currentFileConnectionThread != null)
			currentFileConnectionThread.setRunning(false);
		running.setValue(toSet);
	}

	@Override
	public synchronized SimpleBooleanProperty isFinished() {
		return finished;
	}

	@Override
	public String getNameToDisplay() {
		return baseDir;
	}
}
