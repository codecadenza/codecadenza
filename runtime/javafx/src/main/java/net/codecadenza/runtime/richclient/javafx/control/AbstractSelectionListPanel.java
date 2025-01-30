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

import static javafx.scene.input.MouseEvent.MOUSE_CLICKED;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.DATA_FETCH_ACTION_MSG_QUERY_FAILED;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SELECTION_LIST_PANEL_LBL_SEARCH_ELEMENTS;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SELECTION_LIST_PANEL_LBL_SOURCE_LIST;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SELECTION_LIST_PANEL_LBL_TARGET_LIST;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_DESELECT_ALL;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_DESELECT_ITEM;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_SELECT_ALL;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_SELECT_ITEM;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.getImage;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import javafx.application.Platform;
import javafx.concurrent.Task;
import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.geometry.VPos;
import javafx.scene.Cursor;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.control.TitledPane;
import javafx.scene.control.Tooltip;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.util.Callback;
import net.codecadenza.runtime.richclient.javafx.dialog.DialogUtil;

/**
 * <p>
 * Abstract generic base class for dual data selection list components
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the selection list
 */
public abstract class AbstractSelectionListPanel<T> extends VBox {
	private static final long INPUT_DELAY = 250;

	private ListView<T> sourceListView;
	private ListView<T> targetListView;
	private TextField txtFilter;
	private DataFetchTask task;
	private boolean addSearchField;

	/**
	 * Constructor
	 */
	protected AbstractSelectionListPanel() {
		initialize(null, false);
	}

	/**
	 * Constructor
	 * @param title
	 */
	protected AbstractSelectionListPanel(String title) {
		initialize(title, false);
	}

	/**
	 * Constructor
	 * @param title
	 * @param addSearchField
	 */
	protected AbstractSelectionListPanel(String title, boolean addSearchField) {
		this.addSearchField = addSearchField;

		initialize(title, addSearchField);
	}

	/**
	 * @param element
	 * @return the string representation of a given element
	 */
	public abstract String getItemText(T element);

	/**
	 * @param filter
	 * @return a list containing all items to be displayed in the source list
	 * @throws Exception if the search operation has failed
	 */
	public abstract List<T> searchItems(String filter) throws Exception;

	/**
	 * @return a list containing all available items
	 */
	public List<T> getAvailableItems() {
		return sourceListView.getItems().stream().toList();
	}

	/**
	 * @param availableItems
	 */
	public void setAvailableItems(Collection<T> availableItems) {
		sourceListView.getItems().clear();

		// Remove all items that are already selected!
		sourceListView.getItems().addAll(availableItems.stream().filter(f -> !getSelectedItems().contains(f)).toList());
	}

	/**
	 * @return a list containing all selected items
	 */
	public List<T> getSelectedItems() {
		return targetListView.getItems().stream().toList();
	}

	/**
	 * @param selectedItems
	 */
	public void setSelectedItems(Collection<T> selectedItems) {
		targetListView.getItems().clear();
		targetListView.getItems().addAll(selectedItems);
	}

	/**
	 * Move an item from the source to the target list
	 */
	private void moveToTarget() {
		move(sourceListView, targetListView);

		sourceListView.getSelectionModel().clearSelection();
	}

	/**
	 * Move all items to the target list
	 */
	private void moveAllToTarget() {
		move(sourceListView, targetListView, new ArrayList<>(sourceListView.getItems()));

		sourceListView.getSelectionModel().clearSelection();
	}

	/**
	 * Move an item from the target to the source list
	 */
	private void moveToSource() {
		move(targetListView, sourceListView);

		targetListView.getSelectionModel().clearSelection();
	}

	/**
	 * Move all items to the source list
	 */
	private void moveAllToSource() {
		move(targetListView, sourceListView, new ArrayList<>(targetListView.getItems()));

		targetListView.getSelectionModel().clearSelection();
	}

	/**
	 * Move all items from one list view to another
	 * @param fromList
	 * @param toList
	 */
	private void move(ListView<T> fromList, ListView<T> toList) {
		final List<T> selectedItems = new ArrayList<>(fromList.getSelectionModel().getSelectedItems());

		move(fromList, toList, selectedItems);
	}

	/**
	 * Move a list of items from one list view to another
	 * @param fromList
	 * @param toList
	 * @param items
	 */
	private void move(ListView<T> fromList, ListView<T> toList, List<T> items) {
		items.forEach(item -> {
			fromList.getItems().remove(item);
			toList.getItems().add(item);
		});
	}

	/**
	 * Task for fetching the selection items
	 */
	private class DataFetchTask extends Task<Void> {
		private final String filter;

		/**
		 * Constructor
		 * @param filter
		 */
		public DataFetchTask(String filter) {
			this.filter = filter;
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
		public Void call() throws Exception {
			final long startTime = System.currentTimeMillis();
			final List<T> data;

			if (addSearchField && filter.isEmpty())
				data = Collections.emptyList();
			else
				data = searchItems(filter);

			if (addSearchField) {
				final long sleepTime = startTime + INPUT_DELAY - System.currentTimeMillis();

				// Don't sleep if the operation took longer than the input delay!
				if (sleepTime > 0 && !isDone())
					Thread.sleep(sleepTime);
			}

			if (isDone())
				return null;

			Platform.runLater(() -> setAvailableItems(data));

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
	 * Overwrite the textual representation of a {@link ListCell}
	 */
	private class InternalCallback implements Callback<ListView<T>, ListCell<T>> {
		/*
		 * (non-Javadoc)
		 * @see javafx.util.Callback#call(java.lang.Object)
		 */
		@Override
		public ListCell<T> call(ListView<T> arg0) {
			return new ListCell<>() {
				/*
				 * (non-Javadoc)
				 * @see javafx.scene.control.Cell#updateItem(java.lang.Object, boolean)
				 */
				@Override
				protected void updateItem(T t, boolean bln) {
					super.updateItem(t, bln);

					if (t != null)
						setText(getItemText(t));
					else
						setText("");
				}
			};
		}
	}

	/**
	 * Refresh the source list
	 */
	private void refresh() {
		if (task != null && task.isRunning())
			task.cancel();

		new Thread(task = new DataFetchTask(txtFilter.getText())).start();
	}

	/**
	 * Initialize the component
	 * @param title
	 * @param addSearchField
	 */
	private void initialize(String title, boolean addSearchField) {
		GridPane panFilter = null;

		if (addSearchField) {
			panFilter = new GridPane();
			panFilter.setHgap(5.0);
			panFilter.getColumnConstraints().add(
					new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.SOMETIMES, HPos.LEFT, false));
			panFilter.getColumnConstraints()
					.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.LEFT, true));
			panFilter.add(new Label(getTranslation(SELECTION_LIST_PANEL_LBL_SEARCH_ELEMENTS)), 0, 0);
			panFilter.add(txtFilter = new TextField(), 1, 0);
			panFilter.setPadding(new Insets(5, 0, 10, 0));

			txtFilter.textProperty().addListener((observable, oldValue, newValue) -> refresh());
		}

		final var panLists = new GridPane();
		panLists.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.LEFT, true));
		panLists.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.NEVER, HPos.CENTER, false));
		panLists.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.LEFT, true));
		panLists.setHgap(5);
		panLists.setVgap(5);
		panLists.setPrefHeight(150);
		panLists.setMaxHeight(Double.MAX_VALUE);
		panLists.setMaxWidth(Double.MAX_VALUE);

		final var lblSource = new Label(getTranslation(SELECTION_LIST_PANEL_LBL_SOURCE_LIST));
		final var lblTarget = new Label(getTranslation(SELECTION_LIST_PANEL_LBL_TARGET_LIST));

		sourceListView = new ListView<>();
		sourceListView.setCellFactory(new InternalCallback());

		targetListView = new ListView<>();
		targetListView.setCellFactory(new InternalCallback());

		sourceListView.addEventHandler(MOUSE_CLICKED, event -> {
			if (event.getButton() == MouseButton.PRIMARY && event.getClickCount() == 2)
				moveToTarget();
		});

		targetListView.addEventHandler(MOUSE_CLICKED, event -> {
			if (event.getButton() == MouseButton.PRIMARY && event.getClickCount() == 2)
				moveToSource();
		});

		final var panButtons = new VBox(5);
		panButtons.setFillWidth(true);

		final var cmdSelectItem = new Button("", new ImageView(getImage(IMG_SELECT_ITEM)));
		cmdSelectItem.setOnAction(e -> moveToTarget());

		final var cmdSelectAllItems = new Button("", new ImageView(getImage(IMG_SELECT_ALL)));
		cmdSelectAllItems.setOnAction(e -> moveAllToTarget());

		final var cmdDeselectItem = new Button("", new ImageView(getImage(IMG_DESELECT_ITEM)));
		cmdDeselectItem.setOnAction(e -> moveToSource());

		final var cmdDeselectAllItems = new Button("", new ImageView(getImage(IMG_DESELECT_ALL)));
		cmdDeselectAllItems.setOnAction(e -> moveAllToSource());

		panButtons.getChildren().addAll(cmdSelectItem, cmdSelectAllItems, cmdDeselectItem, cmdDeselectAllItems);

		panLists.add(lblSource, 0, 0);
		panLists.add(lblTarget, 2, 0);
		panLists.add(sourceListView, 0, 1);
		panLists.add(panButtons, 1, 1);
		panLists.add(targetListView, 2, 1);

		GridPane.setValignment(panButtons, VPos.CENTER);
		GridPane.setVgrow(panButtons, Priority.NEVER);
		GridPane.setVgrow(sourceListView, Priority.ALWAYS);
		GridPane.setVgrow(targetListView, Priority.ALWAYS);

		if (title != null) {
			final var panTitleContent = new VBox();

			if (panFilter != null)
				panTitleContent.getChildren().add(panFilter);

			panTitleContent.getChildren().add(panLists);

			final var panTitle = new TitledPane(title, panTitleContent);
			panTitle.setCollapsible(false);
			panTitle.setMaxHeight(Double.MAX_VALUE);
			panTitle.setMaxWidth(Double.MAX_VALUE);

			getChildren().add(panTitle);

			VBox.setVgrow(panTitleContent, Priority.ALWAYS);
			VBox.setVgrow(panLists, Priority.ALWAYS);
			VBox.setVgrow(panTitle, Priority.ALWAYS);
		}
		else {
			if (panFilter != null)
				getChildren().add(panFilter);

			getChildren().add(panLists);

			VBox.setVgrow(panLists, Priority.ALWAYS);
		}
	}

	/**
	 * @param tooltip
	 */
	public void setTooltip(Tooltip tooltip) {
		targetListView.setTooltip(tooltip);
	}

}
