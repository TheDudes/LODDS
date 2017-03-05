package studyproject.gui.mainWindow;

import javax.annotation.PostConstruct;

import studyproject.API.Lvl.Mid.Lodds.Lodds;
import studyproject.gui.Core.Utils;
import javafx.stage.Stage;

public class MainWindowModel {

	@PostConstruct
	public void init(){
		Utils.setLodds(new Lodds());
	}
	
	public Lodds getLodds() {
		return Utils.getLodds();
	}

	public void setLodds(Lodds lodds) {
		Utils.setLodds(lodds);
	}
	
	public Stage getNewStage() {
		return new Stage();
	}
	
}
