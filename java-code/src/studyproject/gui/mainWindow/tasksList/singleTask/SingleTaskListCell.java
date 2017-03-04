package studyproject.gui.mainWindow.tasksList.singleTask;

import javafx.scene.control.ListCell;
import studyproject.API.Lvl.Mid.ThreadMonitoring.MonitoredThread;

/**
 * @author ninti
 */
public class SingleTaskListCell extends ListCell<MonitoredThread> {
	@Override
	protected void updateItem(MonitoredThread monitoredThread, boolean empty) {
		super.updateItem(monitoredThread, empty);
		if (empty || monitoredThread == null) {
			setText(null);
			setGraphic(null);
		} else {
			setText(null);
			SingleTaskView view = new SingleTaskView();
			((SingleTaskPresenter) view.getPresenter()).setMonitoredThread(monitoredThread);
			setGraphic(view.getView());
		}
	}
}
