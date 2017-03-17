package studyproject.gui.introduction;

import java.util.ArrayList;

import javax.annotation.PostConstruct;

public class IntroductionModel {

	private ArrayList<String> discriptions;
	private ArrayList<String> titles;

	@PostConstruct
	public void init() {
		discriptions = new ArrayList<>();
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
