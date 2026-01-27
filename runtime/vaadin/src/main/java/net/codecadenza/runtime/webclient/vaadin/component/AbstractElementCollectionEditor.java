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
package net.codecadenza.runtime.webclient.vaadin.component;

import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.ABSTRACT_ELEMENT_COLLECTION_EDITOR_LBL_ADD;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.ABSTRACT_ELEMENT_COLLECTION_EDITOR_LBL_NUMBER_OF_ELEMENTS;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.ABSTRACT_ELEMENT_COLLECTION_EDITOR_MSG_CONVERSION_FAILED;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.ABSTRACT_ELEMENT_COLLECTION_EDITOR_MSG_TITLE_CONVERSION;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_ADD;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_DELETE;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_DELETE_ALL;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.contextmenu.ContextMenu;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.html.NativeLabel;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.renderer.TextRenderer;
import com.vaadin.flow.data.value.ValueChangeMode;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import net.codecadenza.runtime.conversion.ValueConverter;
import net.codecadenza.runtime.webclient.vaadin.dialog.InfoMessageDialog;
import net.codecadenza.runtime.webclient.vaadin.i18n.I18NService;
import net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService;
import net.codecadenza.runtime.webclient.vaadin.util.PreferencesStore;

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
public abstract class AbstractElementCollectionEditor<T> extends VerticalLayout {
	private static final long serialVersionUID = -5259143711207570405L;

	protected final PreferencesStore preferences;
	protected final transient ValueConverter<T> valueConverter;
	protected final Class<T> elementType;
	protected final Grid<String> grid = new Grid<>();
	protected final InternalI18NService internalI18n;
	protected transient Collection<T> elements = new ArrayList<>();
	protected boolean readOnly;
	protected final NativeLabel lblNumberOfElements = new NativeLabel();

	/**
	 * Constructor
	 * @param readOnly
	 * @param elementType
	 * @param i18n
	 * @param preferences
	 */
	protected AbstractElementCollectionEditor(boolean readOnly, Class<T> elementType, I18NService i18n,
			PreferencesStore preferences) {
		this.elementType = elementType;
		this.readOnly = readOnly;
		this.preferences = preferences;
		this.internalI18n = new InternalI18NService(i18n.getLocale());
		this.valueConverter = new ValueConverter<>(preferences.getNumberFormat(), preferences.getDateTimeFormat(),
				preferences.getDateFormat(), elementType);

		initEditor();
	}

	/**
	 * Initialize the editor
	 */
	protected void initEditor() {
		HorizontalLayout hlAdd = null;

		if (!readOnly) {
			final var txtInput = new TextField();
			txtInput.setValue(valueConverter.getInitialDefaultValue());
			txtInput.setValueChangeMode(ValueChangeMode.LAZY);
			txtInput.addValueChangeListener(event -> refreshGrid(event.getValue()));

			final var cmdAdd = new Button(internalI18n.getTranslation(CMD_ADD));

			cmdAdd.addClickListener(_ -> {
				try {
					final T newElement = valueConverter.convertToValue(txtInput.getValue());

					elements.add(newElement);
					refreshGrid(null);
				}
				catch (final Exception ex) {
					new InfoMessageDialog(internalI18n.getTranslation(ABSTRACT_ELEMENT_COLLECTION_EDITOR_MSG_TITLE_CONVERSION),
							internalI18n.getTranslation(ABSTRACT_ELEMENT_COLLECTION_EDITOR_MSG_CONVERSION_FAILED, ex.getMessage()),
							internalI18n.getLocale()).open();
				}
			});

			hlAdd = new HorizontalLayout();
			hlAdd.add(new NativeLabel(internalI18n.getTranslation(ABSTRACT_ELEMENT_COLLECTION_EDITOR_LBL_ADD)));
			hlAdd.add(txtInput);
			hlAdd.add(cmdAdd);
			hlAdd.setFlexGrow(1, txtInput);
			hlAdd.setWidthFull();
			hlAdd.setDefaultVerticalComponentAlignment(Alignment.CENTER);
		}

		grid.addColumn(new TextRenderer<>());
		grid.setSelectionMode(Grid.SelectionMode.SINGLE);

		if (!readOnly) {
			final var contextMenu = new ContextMenu(grid);

			final var mniDelete = contextMenu.addItem(internalI18n.getTranslation(CMD_DELETE));
			mniDelete.setId("mniDelete");

			mniDelete.addClickListener(_ -> {
				final String selectedItem = grid.getSelectedItems().stream().findFirst().orElse(null);

				if (selectedItem != null) {
					elements.remove(valueConverter.convertToValue(selectedItem));
					refreshGrid(null);
				}
			});

			final var mniDeleteAllItems = contextMenu.addItem(internalI18n.getTranslation(CMD_DELETE_ALL));
			mniDeleteAllItems.setId("mniDeleteAll");

			mniDeleteAllItems.addClickListener(_ -> {
				elements.clear();
				refreshGrid(null);
			});

			add(hlAdd);
		}

		add(grid);
		add(lblNumberOfElements);
	}

	/**
	 * Refresh the {@link Grid} with the elements to be displayed
	 * @param filter the filter for elements to be displayed
	 */
	protected void refreshGrid(String filter) {
		final String msgNumberOfElements = internalI18n.getTranslation(ABSTRACT_ELEMENT_COLLECTION_EDITOR_LBL_NUMBER_OF_ELEMENTS,
				elements.size());
		final List<String> filteredElements;

		if (filter != null && !filter.isEmpty())
			filteredElements = elements.stream().sorted().map(valueConverter::convertToString).filter(item -> item.startsWith(filter))
					.toList();
		else
			filteredElements = elements.stream().sorted().map(valueConverter::convertToString).toList();

		lblNumberOfElements.setText(msgNumberOfElements);
		grid.setItems(filteredElements);
	}

}
