package studyproject.gui.introduction;

import java.util.ArrayList;

import javax.annotation.PostConstruct;

import javafx.beans.property.SimpleIntegerProperty;

public class IntroductionModel {

	private ArrayList<String> discriptions;
	private ArrayList<String> titles;
	private SimpleIntegerProperty index;

	@PostConstruct
	public void init() {
		discriptions = new ArrayList<>();
		titles = new ArrayList<>();
		index = new SimpleIntegerProperty(0);
	}

	public SimpleIntegerProperty getIndex() {
		return index;
	}

	public void setIndex(SimpleIntegerProperty index) {
		this.index = index;
	}

	public ArrayList<String> getDiscriptions() {
		return discriptions;
	}

	public void setDiscriptions(ArrayList<String> discriptions) {
		this.discriptions = discriptions;
	}

	public ArrayList<String> getTitles() {
		return titles;
	}

	public void setTitles(ArrayList<String> titles) {
		this.titles = titles;
	}

}
