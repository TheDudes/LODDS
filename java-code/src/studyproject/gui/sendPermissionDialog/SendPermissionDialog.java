package studyproject.gui.sendPermissionDialog;

import java.util.Timer;

import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.stage.Stage;
import studyproject.API.Core.Request.GetPermissionRequest;

/**
 * JavaFX Stage Wrapper. This Wrapper is used like a normal Stage with an
 * additional GetPermissionRequest variable
 * @author Chris
 *
 */
public class SendPermissionDialog extends Stage {
	private GetPermissionRequest getPermissionRequest = null;
	private SimpleStringProperty labelText = null;
	private SimpleDoubleProperty progressDouble = null;
	private Timer timer = null;

	public SendPermissionDialog(GetPermissionRequest getPermissionRequest, String labelText) {
		this.getPermissionRequest = getPermissionRequest;
		this.labelText = new SimpleStringProperty(labelText);
		this.progressDouble = new SimpleDoubleProperty(1.0);
		timer = new Timer();
//		timer.scheduleAtFixedRate(new TimerTask() {
//	        @Override
//	        public void run() {
//	        	decreaseProgress(timer, 1.00 / getPermissionRequest.timeout);
//	        }
//	    }, 0, 1000);
	}
	
	public GetPermissionRequest getPermissionRequest() {
		return getPermissionRequest;
	}

	public Timer getTimer() {
		return timer;
	}
	
	public SimpleStringProperty getLabelText() {
		return labelText;
	}
	
	public SimpleDoubleProperty getProgressDouble() {
		return progressDouble;
	}

	public void setPermissionRequest(GetPermissionRequest getPermissionRequest) {
		this.getPermissionRequest = getPermissionRequest;
	}
	
	public void setProgressDouble(double d) {
		progressDouble.set(d);
	}
}
