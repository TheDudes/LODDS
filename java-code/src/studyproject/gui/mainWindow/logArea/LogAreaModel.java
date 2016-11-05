package studyproject.gui.mainWindow.logArea;

import javax.annotation.PostConstruct;

import javafx.beans.property.SimpleBooleanProperty;


public class LogAreaModel {

	@PostConstruct
	public void init() {
	}
	
	private SimpleBooleanProperty error = new SimpleBooleanProperty();
	private SimpleBooleanProperty info = new SimpleBooleanProperty();
	private SimpleBooleanProperty getRec = new SimpleBooleanProperty();
	private SimpleBooleanProperty getSent = new SimpleBooleanProperty();
	private SimpleBooleanProperty broadcast = new SimpleBooleanProperty();
	
	
	
	public SimpleBooleanProperty getInfo() {
		return info;
	}

	public void setInfo(SimpleBooleanProperty info) {
		this.info = info;
	}

	public SimpleBooleanProperty getGetRec() {
		return getRec;
	}

	public void setGetRec(SimpleBooleanProperty getRec) {
		this.getRec = getRec;
	}

	public SimpleBooleanProperty getGetSent() {
		return getSent;
	}

	public void setGetSent(SimpleBooleanProperty getSent) {
		this.getSent = getSent;
	}

	public SimpleBooleanProperty getBroadcast() {
		return broadcast;
	}

	public void setBroadcast(SimpleBooleanProperty broadcast) {
		this.broadcast = broadcast;
	}

	public SimpleBooleanProperty getError() {
		return error;
	}

	public void setError(SimpleBooleanProperty error) {
		this.error = error;
	}
	
	
	
	

	
}
