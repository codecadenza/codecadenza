/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.runtime.richclient.javafx.control;

import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.DATA_FETCH_ACTION_MSG_QUERY_FAILED;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.concurrent.Task;
import javafx.scene.Cursor;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.stage.Popup;
import net.codecadenza.runtime.richclient.javafx.dialog.DialogUtil;

/**
 * <p>
 * Abstract proposal text field
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the proposal text field
 */
public abstract class AbstractProposalTextField<T> extends TextField {
	private static final long AUTO_COMPLETE_DELAY = 250;
	private static final int ITEM_HEIGHT = 26;
	private static final int MAX_POPUP_HEIGHT = 5 * ITEM_HEIGHT;
	private static final int DEFAULT_ITEM_LIMIT = 6;

	private ListView<T> listView;
	private Popup popup;
	private int limit;
	private long autoCompleteDelay;
	private T selectedItem;
	private DataFetchTask dataFetchTask;
	private FilterInputChangeListener inputChangeListener;

	/**
	 * Constructor
	 */
	protected AbstractProposalTextField() {
		this(DEFAULT_ITEM_LIMIT, AUTO_COMPLETE_DELAY);
	}

	/**
	 * Constructor
	 * @param itemLimit the maximum number of items to be displayed in proposal list view
	 * @param autoCompleteDelay the auto-complete delay in milliseconds
	 */
	protected AbstractProposalTextField(int itemLimit, long autoCompleteDelay) {
		this.limit = itemLimit;
		this.autoCompleteDelay = autoCompleteDelay;

		listView = new ListView<>();
		listView.setOnMouseReleased(e -> selectItem());

		listView.itemsProperty().addListener((ChangeListener<ObservableList<T>>) (value, oldValue, newValue) -> {
			if (listView.getItems() != null && !listView.getItems().isEmpty())
				showPopup();
			else
				hidePopup();
		});

		listView.setOnKeyReleased(e -> {
			if (e.getCode() == KeyCode.ENTER)
				selectItem();
		});

		listView.setCellFactory(p -> new ListCell<>() {
			/*
			 * (non-Javadoc)
			 * @see javafx.scene.control.Cell#updateItem(java.lang.Object, boolean)
			 */
			@Override
			public void updateItem(T item, boolean empty) {
				super.updateItem(item, empty);

				if (item != null)
					setText(getProposalText(item));
				else
					setText("");
			}
		});

		textProperty().addListener(inputChangeListener = new FilterInputChangeListener());

		popup = new Popup();
		popup.setAutoHide(true);
		popup.getContent().add(listView);
	}

	/**
	 * @param textEntered
	 * @return a list of proposals
	 * @throws Exception if the data fetch operation has failed
	 */
	@SuppressWarnings("unused")
	public List<T> getProposalItems(String textEntered) throws Exception {
		return new ArrayList<>();
	}

	/**
	 * @param item
	 * @return the proposal text
	 */
	public abstract String getProposalText(T item);

	/**
	 * Apply the selected object
	 * @param item
	 */
	public void applySelection(T item) {
		selectedItem = item;

		// When selecting an item the respective text field change listener must be removed temporarily!
		textProperty().removeListener(inputChangeListener);

		if (item != null)
			setText(getProposalText(item));
		else
			setText("");

		textProperty().addListener(inputChangeListener);
	}

	/**
	 * @return the selected item
	 */
	public T getSelectedItem() {
		return selectedItem;
	}

	/**
	 * @param selectedItem
	 */
	public void setSelectedItem(T selectedItem) {
		this.selectedItem = selectedItem;

		applySelection(selectedItem);
	}

	/**
	 * Select the item from the list view
	 */
	private void selectItem() {
		applySelection(listView.getSelectionModel().getSelectedItem());

		if (selectedItem == null)
			return;

		listView.getItems().clear();

		requestFocus();
		requestLayout();
		end();
		hidePopup();
	}

	/**
	 * Show the pop-up control
	 */
	public void showPopup() {
		final int itemCount = listView.getItems().size();

		listView.setPrefWidth(getWidth());
		listView.setPrefHeight(itemCount * ITEM_HEIGHT > MAX_POPUP_HEIGHT ? MAX_POPUP_HEIGHT : itemCount * ITEM_HEIGHT);

		final double xPos = getScene().getWindow().getX() + localToScene(0, 0).getX() + getScene().getX();
		final double yPos = getScene().getWindow().getY() + localToScene(0, 0).getY() + getScene().getY() + ITEM_HEIGHT;

		popup.show(getScene().getWindow(), xPos, yPos);

		listView.getSelectionModel().clearSelection();
		listView.getFocusModel().focus(-1);
	}

	/**
	 * Hide the pop-up control
	 */
	public void hidePopup() {
		popup.hide();
	}

	/**
	 * Task for fetching proposal items asynchronously
	 */
	private class DataFetchTask extends Task<Void> {
		private final String filterText;

		/**
		 * Constructor
		 * @param filterText
		 */
		public DataFetchTask(String filterText) {
			this.filterText = filterText;
		}

		/*
		 * (non-Javadoc)
		 * @see javafx.concurrent.Task#scheduled()
		 */
		@Override
		protected void scheduled() {
			getScene().setCursor(Cursor.WAIT);
		}

		/*
		 * (non-Javadoc)
		 * @see javafx.concurrent.Task#call()
		 */
		@Override
		protected Void call() throws Exception {
			final long startTime = System.currentTimeMillis();
			final ObservableList<T> list;

			if (filterText.isEmpty())
				list = FXCollections.observableArrayList();
			else
				list = getProposalItems(filterText).stream().limit(limit)
						.collect(Collectors.toCollection(FXCollections::observableArrayList));

			final long sleepTime = startTime + autoCompleteDelay - System.currentTimeMillis();

			// Don't sleep if the operation took longer than the auto-complete delay!
			if (sleepTime > 0 && !isDone())
				Thread.sleep(sleepTime);

			if (isDone())
				return null;

			Platform.runLater(() -> {
				if (list != null && !list.isEmpty()) {
					listView.setItems(list);

					showPopup();
				}
				else
					hidePopup();
			});

			return null;
		}

		/*
		 * (non-Javadoc)
		 * @see java.util.concurrent.FutureTask#done()
		 */
		@Override
		protected void done() {
			Platform.runLater(() -> getScene().setCursor(Cursor.DEFAULT));
		}

		/*
		 * (non-Javadoc)
		 * @see javafx.concurrent.Task#failed()
		 */
		@Override
		protected void failed() {
			DialogUtil.openErrorDialog(null, getTranslation(DATA_FETCH_ACTION_MSG_QUERY_FAILED), getException());
		}
	}

	/**
	 * Listener for filter input changes
	 */
	private class FilterInputChangeListener implements ChangeListener<String> {
		/*
		 * (non-Javadoc)
		 * @see javafx.beans.value.ChangeListener#changed(javafx.beans.value.ObservableValue, java.lang.Object, java.lang.Object)
		 */
		@Override
		public void changed(ObservableValue<? extends String> observable, String newValue, String oldValue) {
			if (getText().length() > 0) {
				if (dataFetchTask != null && dataFetchTask.isRunning())
					dataFetchTask.cancel();

				new Thread(dataFetchTask = new DataFetchTask(getText())).start();
			}
			else
				hidePopup();
		}
	}

}
