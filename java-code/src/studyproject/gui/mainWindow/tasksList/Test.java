package studyproject.gui.mainWindow.tasksList;

import javafx.concurrent.Task;

public class Test extends Task<Void> {

	@Override
	protected Void call() throws Exception {

		while (true) {
			System.out.println("blub test task");

			return null;
		}

	}
}
