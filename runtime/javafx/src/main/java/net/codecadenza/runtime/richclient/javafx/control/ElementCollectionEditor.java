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

import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_DELETE_ALL_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_DELETE_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.CMD_ADD;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ELEMENT_COLLECTION_EDITOR_LBL_ADD;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ELEMENT_COLLECTION_EDITOR_MSG_CONVERSION_FAILED;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ELEMENT_COLLECTION_EDITOR_MSG_TITLE_CONVERSION;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslationForFieldLabel;

import java.text.DecimalFormat;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TextField;
import javafx.scene.control.TitledPane;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import net.codecadenza.runtime.conversion.ValueConverter;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.javafx.dialog.DialogUtil;

/**
 * <p>
 * Editor for maintaining element collections
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of an element in the collection
 */
public class ElementCollectionEditor<T> extends VBox {
	protected final FormatDTO userFormat = FormatPreferencesManager.getFormatDTO();
	protected final DateTimeFormatter dateTimeFormat = DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat())
			.withZone(ZoneId.systemDefault());
	protected final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(userFormat.getDateFormat())
			.withZone(ZoneId.systemDefault());
	protected final DecimalFormat decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
	protected final boolean readonly;
	protected final Class<T> elementType;
	protected final ValueConverter<T> valueConverter;
	protected final ObservableList<String> observableList = FXCollections.observableArrayList();
	protected Collection<T> elements = Collections.emptyList();
	protected ListView<String> elementList;

	/**
	 * Constructor
	 * @param title
	 * @param readonly
	 * @param elementType
	 */
	public ElementCollectionEditor(String title, boolean readonly, Class<T> elementType) {
		this.readonly = readonly;
		this.elementType = elementType;
		this.valueConverter = new ValueConverter<>(userFormat.getDecimalFormat(), userFormat.getDateTimeFormat(),
				userFormat.getDateFormat(), elementType);

		initControl(title);
	}

	/**
	 * Constructor
	 * @param readonly
	 * @param elementType
	 */
	public ElementCollectionEditor(boolean readonly, Class<T> elementType) {
		this(null, readonly, elementType);
	}

	/**
	 * Set the initial elements
	 * @param elements the initial elements
	 */
	public void setElements(Collection<T> elements) {
		this.elements = elements;

		refreshListView(null);
	}

	/**
	 * Initialize the control
	 * @param title
	 */
	protected void initControl(String title) {
		GridPane panAdd = null;

		if (!readonly) {
			panAdd = new GridPane();
			panAdd.setHgap(5.0);
			panAdd.getColumnConstraints().add(
					new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.SOMETIMES, HPos.LEFT, false));
			panAdd.getColumnConstraints()
					.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.LEFT, true));
			panAdd.add(new Label(getTranslationForFieldLabel(ELEMENT_COLLECTION_EDITOR_LBL_ADD)), 0, 0);

			final var txtAdd = new TextField();
			txtAdd.setText(valueConverter.getInitialDefaultValue());
			txtAdd.textProperty().addListener((observable, oldValue, newValue) -> refreshListView(newValue));

			final var cmdAdd = new Button(getTranslation(CMD_ADD));

			cmdAdd.setOnAction(e -> {
				if (txtAdd.getText().isEmpty())
					return;

				try {
					final T newElement = valueConverter.convertToValue(txtAdd.getText());

					elements.add(newElement);
					refreshListView(null);
				}
				catch (final Exception ex) {
					DialogUtil.openInformationDialog(null, getTranslation(ELEMENT_COLLECTION_EDITOR_MSG_TITLE_CONVERSION),
							getTranslation(ELEMENT_COLLECTION_EDITOR_MSG_CONVERSION_FAILED, ex.getMessage()));
					return;
				}
			});

			panAdd.add(txtAdd, 1, 0);
			panAdd.add(cmdAdd, 2, 0);
			panAdd.setPadding(new Insets(0, 0, 10, 0));
		}

		elementList = new ListView<>();
		elementList.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
		elementList.setPrefHeight(200);
		elementList.setMaxHeight(Double.MAX_VALUE);
		elementList.setMaxWidth(Double.MAX_VALUE);
		elementList.setItems(observableList);

		if (!readonly) {
			elementList.setOnKeyPressed(e -> {
				if (e.getCode() == KeyCode.DELETE)
					deleteSelectedItem();
			});

			final var mniDelete = new MenuItem(getTranslation(ACTION_DELETE_TITLE));
			mniDelete.setOnAction(action -> deleteSelectedItem());

			final var mniDeleteAll = new MenuItem(getTranslation(ACTION_DELETE_ALL_TITLE));
			mniDeleteAll.setOnAction(action -> {
				elements.clear();
				refreshListView(null);
			});

			final var contextMenu = new ContextMenu();
			contextMenu.getItems().add(mniDelete);
			contextMenu.getItems().add(mniDeleteAll);

			elementList.setContextMenu(contextMenu);
		}

		if (title != null) {
			final var panTitleContent = new VBox();

			if (panAdd != null)
				panTitleContent.getChildren().add(panAdd);

			panTitleContent.getChildren().add(elementList);

			final var panTitle = new TitledPane(title, panTitleContent);
			panTitle.setCollapsible(false);
			panTitle.setMaxHeight(Double.MAX_VALUE);
			panTitle.setMaxWidth(Double.MAX_VALUE);

			getChildren().add(panTitle);

			VBox.setVgrow(panTitleContent, Priority.ALWAYS);
			VBox.setVgrow(elementList, Priority.ALWAYS);
			VBox.setVgrow(panTitle, Priority.ALWAYS);
		}
		else {
			if (panAdd != null)
				getChildren().add(panAdd);

			getChildren().add(elementList);

			VBox.setVgrow(elementList, Priority.ALWAYS);
		}
	}

	/**
	 * Refresh the {@link ListView} that contains the visual representation of the elements
	 * @param filter the filter for elements to be displayed
	 */
	protected void refreshListView(String filter) {
		final List<String> filteredElements;

		if (filter != null && !filter.isEmpty())
			filteredElements = elements.stream().sorted().map(valueConverter::convertToString).filter(item -> item.startsWith(filter))
					.toList();
		else
			filteredElements = elements.stream().sorted().map(valueConverter::convertToString).toList();

		observableList.clear();
		observableList.addAll(filteredElements);
	}

	/**
	 * Delete the selected item
	 */
	protected void deleteSelectedItem() {
		final String selectedItem = elementList.getSelectionModel().getSelectedItem();

		if (selectedItem != null) {
			elements.remove(valueConverter.convertToValue(selectedItem));
			refreshListView(null);
		}
	}
}
