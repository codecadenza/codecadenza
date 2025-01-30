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
package net.codecadenza.runtime.richclient.javafx.dialog;

import static javafx.scene.layout.Region.USE_COMPUTED_SIZE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_LOV_DIALOG_INFO_MESSAGE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_LOV_DIALOG_LBL_FILTER;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_LOV_DIALOG_SELECT;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_LOV_DIALOG_TITLE_MESSAGE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.CMD_RESET;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslationForFieldLabel;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_LOV_TITLE;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.getImage;

import java.text.DecimalFormat;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.List;
import javafx.collections.ObservableList;
import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Window;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel;
import org.slf4j.Logger;

/**
 * <p>
 * Base class for list-of-values dialogs
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the LOV dialog
 */
public abstract class AbstractLOVDialog<T> extends TitleAreaDialog {
	protected FormatDTO userFormat = FormatPreferencesManager.getFormatDTO();
	protected DateTimeFormatter dateTimeFormat = DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat())
			.withZone(ZoneId.systemDefault());
	protected DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(userFormat.getDateFormat())
			.withZone(ZoneId.systemDefault());
	protected DecimalFormat decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
	protected TableView<T> tableView;
	protected TextField txtFilter;
	private AbstractDataGridPanel<T> gridPanel;
	private T selection;
	private String filterText;
	private boolean enableReset;

	/**
	 * Constructor
	 * @param owner
	 * @param title
	 */
	protected AbstractLOVDialog(Window owner, String title) {
		this(owner, title, false);
	}

	/**
	 * Constructor
	 * @param owner
	 * @param title
	 * @param enableReset
	 */
	protected AbstractLOVDialog(Window owner, String title, boolean enableReset) {
		super(owner, title);

		this.enableReset = enableReset;

		initModality(Modality.APPLICATION_MODAL);
	}

	/**
	 * @param element
	 * @param colIndex
	 * @return the text of a given cell
	 */
	protected abstract String getCellText(T element, int colIndex);

	/**
	 * Search for items by using the given filter
	 * @param filter
	 * @return a list containing all objects found
	 * @throws Exception if the data fetch operation has failed
	 */
	public abstract List<T> fetchData(String filter) throws Exception;

	/**
	 * Initialize the columns
	 * @return a list containing all columns
	 */
	public abstract ObservableList<TableColumn<T, String>> initColumns();

	/**
	 * Every non-abstract subclass must provide a logger instance!
	 * @return the logger
	 */
	protected abstract Logger getLogger();

	/**
	 * Refresh the view
	 */
	private void refresh() {
		gridPanel.refreshView();
	}

	/**
	 * Callback method for table view selection events
	 */
	private void onSelect() {
		selection = tableView.getSelectionModel().getSelectedItem();

		if (selection != null) {
			returnCode = DialogButtonType.OK;
			close();
		}
	}

	/**
	 * Reset the selection
	 */
	private void onReset() {
		selection = null;
		returnCode = DialogButtonType.RESET;

		close();
	}

	/**
	 * @return the selected item
	 */
	public T getSelection() {
		return selection;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#onOKPressed()
	 */
	@Override
	protected void onOKPressed() {
		onSelect();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#createButtons()
	 */
	@Override
	protected void createButtons() {
		super.createButtons();

		if (enableReset) {
			final Button cmdReset = addButton(DialogButtonType.RESET, getTranslation(CMD_RESET), false, false);
			cmdReset.setOnAction(e -> onReset());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#createDialogArea()
	 */
	@Override
	protected Node createDialogArea() {
		getLogger().debug("Initialize dialog");

		setTitleImage(getImage(IMG_LOV_TITLE));
		setTitleMessage(getTranslation(ABSTRACT_LOV_DIALOG_TITLE_MESSAGE));
		setInfoMessage(getTranslation(ABSTRACT_LOV_DIALOG_INFO_MESSAGE));

		final var panDlg = new VBox();

		final var panFilter = new GridPane();
		panFilter.setHgap(5.0);
		panFilter.getColumnConstraints().add(
				new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.SOMETIMES, HPos.LEFT, false));
		panFilter.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.LEFT, true));
		panFilter.add(new Label(getTranslationForFieldLabel(ABSTRACT_LOV_DIALOG_LBL_FILTER)), 0, 0);

		txtFilter = new TextField();

		panFilter.add(txtFilter, 1, 0);
		panFilter.setPadding(new Insets(0, 5, 0, 10));

		gridPanel = new AbstractDataGridPanel<>(this) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#getCellText(java.lang.Object, int)
			 */
			@Override
			protected String getCellText(T element, int colIndex) {
				return AbstractLOVDialog.this.getCellText(element, colIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#initColumns()
			 */
			@Override
			protected ObservableList<TableColumn<T, String>> initColumns() {
				return AbstractLOVDialog.this.initColumns();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#fetchData()
			 */
			@Override
			protected List<T> fetchData() throws Exception {
				if (filterText.isEmpty())
					return Collections.emptyList();

				return AbstractLOVDialog.this.fetchData(filterText);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#onDoubleClick(java.lang.Object)
			 */
			@Override
			protected void onDoubleClick(T item) {
				onSelect();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#getLogger()
			 */
			@Override
			protected Logger getLogger() {
				return AbstractLOVDialog.this.getLogger();
			}
		};

		gridPanel.initialize();

		tableView = gridPanel.getTableView();

		final var item = new MenuItem(getTranslation(ABSTRACT_LOV_DIALOG_SELECT));
		item.setOnAction(e -> onSelect());

		gridPanel.getContextMenu().getItems().add(item);
		gridPanel.initActions();

		panDlg.getChildren().add(panFilter);
		panDlg.getChildren().add(gridPanel);

		VBox.setVgrow(gridPanel, Priority.ALWAYS);

		gridPanel.getTableView().getColumns().addAll(initColumns());

		txtFilter.textProperty().addListener((observable, oldValue, newValue) -> {
			filterText = newValue;

			if (filterText.isEmpty()) {
				gridPanel.getTableView().getItems().clear();
				gridPanel.getStatusBar().setText("");

				return;
			}

			refresh();
		});

		getLogger().debug("Dialog initialization finished");

		return panDlg;
	}

}
