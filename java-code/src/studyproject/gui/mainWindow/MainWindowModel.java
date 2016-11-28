package studyproject.gui.mainWindow;

import javax.annotation.PostConstruct;

import studyproject.API.Lvl.Mid.Lodds.Lodds;

public class MainWindowModel {
	private Lodds lodds;

	@PostConstruct
	public void init(){
		lodds = new Lodds();
	}
	
	public Lodds getLodds() {
		return lodds;
	}

	public void setLodds(Lodds lodds) {
		this.lodds = lodds;
	}
	
}
