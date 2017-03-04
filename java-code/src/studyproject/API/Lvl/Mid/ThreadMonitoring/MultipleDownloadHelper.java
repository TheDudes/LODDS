package studyproject.API.Lvl.Mid.ThreadMonitoring;

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
		for (FileCoreInfo currentFile : assignedDownloads) {
			currentDownloadedFile.set(currentFile.getFileName());
			FileConnectionThread fileConnectionThread = new FileConnectionThread(user, currentFile.getChecksum(),
					currentFile.getFilesize(), pathToDownloadTo + currentFile.getFilePath());
			fileConnectionThread.setOneOfMultiple(true);
			fileConnectionThread.getDoneSize().addListener((observable, oldValue, newValue) -> {
				doneSize.setValue(doneSize.get() + (newValue.longValue() - oldValue.longValue()));
				progress.set((double) doneSize.get() / (double) wholeSize);
			});
			fileConnectionThread.start();
			try {
				fileConnectionThread.join();
			} catch (InterruptedException e) {
				logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, e));
			}
		}
	}

	@Override
	public SimpleStringProperty getCurrentFileName() {
		return currentDownloadedFile;
	}

	@Override
	public SimpleLongProperty getDoneSize() {
		return doneSize;
	}

	@Override
	public long getWholeSize() {
		return wholeSize;
	}

	@Override
	public boolean isSubmitted() {
		return submitted;
	}

	@Override
	public void setSubmitted(boolean toSet) {
		this.submitted = toSet;
	}

	@Override
	public void setProgress(SimpleDoubleProperty toSet) {
		this.progress = toSet;
	}

	@Override
	public SimpleDoubleProperty getProgress() {
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
	public String getNameToDisplay() {
		return baseDir;
	}
}
