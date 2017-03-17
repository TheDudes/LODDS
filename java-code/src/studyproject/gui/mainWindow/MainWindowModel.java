package studyproject.gui.mainWindow;

import javax.annotation.PostConstruct;

import studyproject.API.Lvl.Mid.Lodds.Lodds;
import studyproject.gui.introduction.IntroductionInterface;

public class MainWindowModel implements IntroductionInterface {

	private Lodds lodds;

	@PostConstruct
	public void init() {
		lodds = new Lodds();
	}

	public Lodds getLodds() {
		return lodds;
	}

	public void setLodds(Lodds lodds) {
		this.lodds = lodds;
	}

	@Override
	public String getViewDiscription() {
		return "This is the Main Window discription";
	}

	@Override
	public String getViewTitle() {
		return "Main Window";
	}
}
