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

import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.ABSTRACT_DATA_GRID_ACTION_REFRESH;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_EXPORT;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_REFRESH;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_DATA_FETCH;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_DATA_FETCH_NO_COUNT;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_DATA_FETCH_NO_RESULTS;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_DATA_FETCH_WITH_COUNT;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_REFRESH_ERROR;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_UNEXPECTED_ERROR_TITLE;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.Grid.Column;
import com.vaadin.flow.component.grid.Grid.SelectionMode;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.grid.contextmenu.GridContextMenu;
import com.vaadin.flow.component.html.Anchor;
import com.vaadin.flow.component.html.NativeLabel;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.function.ValueProvider;
import com.vaadin.flow.server.StreamResource;
import jakarta.annotation.PostConstruct;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import net.codecadenza.runtime.webclient.vaadin.dialog.ErrorMessageDialog;
import net.codecadenza.runtime.webclient.vaadin.i18n.I18NService;
import net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService;
import net.codecadenza.runtime.webclient.vaadin.provider.value.DateValueProvider;
import net.codecadenza.runtime.webclient.vaadin.provider.value.DoubleValueProvider;
import net.codecadenza.runtime.webclient.vaadin.provider.value.EnumValueProvider;
import net.codecadenza.runtime.webclient.vaadin.provider.value.FloatValueProvider;
import net.codecadenza.runtime.webclient.vaadin.provider.value.GregorianCalendarValueProvider;
import net.codecadenza.runtime.webclient.vaadin.provider.value.LocalDateTimeValueProvider;
import net.codecadenza.runtime.webclient.vaadin.provider.value.LocalDateValueProvider;
import net.codecadenza.runtime.webclient.vaadin.util.Navigator;
import net.codecadenza.runtime.webclient.vaadin.util.PreferencesStore;
import net.codecadenza.runtime.webclient.vaadin.util.XLSXExportUtility;
import org.slf4j.Logger;

/**
 * <p>
 * Abstract data grid
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the data grid
 */
public abstract class AbstractDataGrid<T> extends VerticalLayout {
	private static final long serialVersionUID = 7083412506796323089L;
	private static final String EXPORT_FILE_NAME = "DataExport.xlsx";
	private static final String ATTRIBUTE_ROW_COUNT = "row-count";

	protected final I18NService i18n;
	protected final PreferencesStore preferences;
	protected final transient Map<String, ValueProvider<T, ?>> columnValueProviders = new HashMap<>();
	protected final Navigator navigator = new Navigator(this);
	protected final Grid<T> grid = new Grid<>();
	protected final GridContextMenu<T> contextMenu = grid.addContextMenu();
	protected final NativeLabel lblFooter = new NativeLabel();
	protected transient List<T> data = new ArrayList<>();
	protected boolean addButtonBar;
	protected boolean addMargin;
	protected InternalI18NService internalI18n;

	/**
	 * Constructor
	 * @param i18n
	 * @param preferences
	 */
	protected AbstractDataGrid(I18NService i18n, PreferencesStore preferences) {
		this.i18n = i18n;
		this.preferences = preferences;
	}

	/**
	 * @param addButtonBar
	 */
	public void setAddButtonBar(boolean addButtonBar) {
		this.addButtonBar = addButtonBar;
	}

	/**
	 * Initialize the grid columns
	 */
	public abstract void initGridColumns();

	/**
	 * Every non-abstract subclass must provide a logger instance!
	 * @return the logger
	 */
	protected abstract Logger getDataGridLogger();

	/**
	 * Initialize the data grid component
	 */
	@PostConstruct
	public void initComponent() {
		getDataGridLogger().debug("Initialize component");

		internalI18n = new InternalI18NService(i18n.getLocale());

		grid.setSizeFull();
		grid.setColumnReorderingAllowed(true);
		grid.setSelectionMode(SelectionMode.SINGLE);
		grid.addThemeVariants(GridVariant.LUMO_COMPACT, GridVariant.LUMO_ROW_STRIPES);
		grid.getElement().setAttribute(ATTRIBUTE_ROW_COUNT, Long.toString(0));

		grid.addItemDoubleClickListener(event -> {
			if (event.getItem() != null) {
				event.getSource().select(event.getItem());
				onDoubleClick(event.getItem());
			}
		});

		if (addButtonBar) {
			final var hlButtons = new HorizontalLayout();

			addButtonsToButtonBar(hlButtons);

			add(hlButtons);
		}

		addContextMenuItems();

		lblFooter.setText(internalI18n.getTranslation(MSG_DATA_FETCH_NO_RESULTS));

		add(grid, lblFooter, contextMenu);

		setSizeFull();

		getDataGridLogger().debug("Component initialization finished");
	}

	/**
	 * @return a list containing all items to be displayed in the table
	 */
	public List<T> fetchData() {
		return new ArrayList<>();
	}

	/**
	 * @param item
	 */
	public void onDoubleClick(@SuppressWarnings("unused") T item) {

	}

	/**
	 * @return the selected item or null if no item is selected
	 */
	public T getSelectedItem() {
		return grid.getSelectedItems().stream().findFirst().orElse(null);
	}

	/**
	 * @param data
	 */
	public void setData(List<T> data) {
		this.data = data;

		grid.setItems(data);

		lblFooter.setText(buildFooterMessage((long) data.size(), null, null));
	}

	/**
	 * Remove the given item from the grid
	 * @param item
	 */
	public void removeItem(T item) {
		data.remove(item);

		setData(data);

		grid.getDataProvider().refreshAll();
	}

	/**
	 * @param numberOfRecords
	 * @param countResult
	 * @param timeDif
	 * @return the generated footer message
	 */
	public String buildFooterMessage(Long numberOfRecords, Long countResult, Long timeDif) {
		final var params = new ArrayList<>();
		params.add(numberOfRecords);

		// Track the number of rows in a custom attribute in order to ease testing!
		grid.getElement().setAttribute(ATTRIBUTE_ROW_COUNT, Long.toString(numberOfRecords));

		if (timeDif == null)
			return internalI18n.getTranslation(MSG_DATA_FETCH, params.toArray());

		if (countResult != null) {
			params.add(countResult);
			params.add(String.format("%.2f", (double) timeDif / 1000));

			return internalI18n.getTranslation(MSG_DATA_FETCH_WITH_COUNT, params.toArray());
		}

		params.add(String.format("%.2f", (double) timeDif / 1000));

		return internalI18n.getTranslation(MSG_DATA_FETCH_NO_COUNT, params.toArray());
	}

	/**
	 * @param hlButtons
	 */
	public void addButtonsToButtonBar(HorizontalLayout hlButtons) {
		final var cmdRefresh = new Button(internalI18n.getTranslation(CMD_REFRESH));
		cmdRefresh.setIcon(new Icon(VaadinIcon.REFRESH));
		cmdRefresh.addClickListener(event -> refresh());
		cmdRefresh.setId(getId().orElseThrow() + "_cmdRefresh");

		final var cmdExport = new Button(internalI18n.getTranslation(CMD_EXPORT));
		cmdExport.setIcon(new Icon(VaadinIcon.FILE_TABLE));

		final var anchorDownload = new Anchor(
				new StreamResource(EXPORT_FILE_NAME, () -> new XLSXExportUtility<>(grid, data, columnValueProviders).createResource()),
				"");
		anchorDownload.getElement().setAttribute("download", true);
		anchorDownload.add(cmdExport);
		anchorDownload.setId(getId().orElseThrow() + "_cmdExport");

		hlButtons.add(cmdRefresh, anchorDownload);
	}

	/**
	 * Reload data and update all respective components
	 */
	public void refresh() {
		try {
			getDataGridLogger().debug("Perform data fetch operation");

			final Long startTime = System.currentTimeMillis();

			data = fetchData();

			grid.setItems(data);

			final Long endTime = System.currentTimeMillis();
			final Long timeDif = endTime - startTime;

			lblFooter.setText(buildFooterMessage((long) data.size(), null, timeDif));

			getDataGridLogger().debug("Data fetch operation finished");
		}
		catch (final Exception e) {
			getDataGridLogger().error("Error while fetching data!", e);

			final String title = internalI18n.getTranslation(MSG_UNEXPECTED_ERROR_TITLE);
			final String message = internalI18n.getTranslation(MSG_REFRESH_ERROR);

			new ErrorMessageDialog(title, message, e, internalI18n.getLocale()).open();
		}
	}

	/**
	 * @return the grid component
	 */
	public Grid<T> getGrid() {
		return grid;
	}

	/**
	 * Add the context menu items
	 */
	protected void addContextMenuItems() {
		contextMenu.addItem(internalI18n.getTranslation(ABSTRACT_DATA_GRID_ACTION_REFRESH), item -> refresh());
	}

	/**
	 * Add a column to the grid
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	public <V> Column<T> addColumn(String columnKey, ValueProvider<T, V> valueProvider) {
		columnValueProviders.put(columnKey, valueProvider);

		return grid.addColumn(valueProvider).setKey(columnKey).setResizable(true).setSortable(true);
	}

	/**
	 * Add a column that is mapped to a field of type {@link Date}. The field values will be converted by using the user's date
	 * format.
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	public Column<T> addDateColumn(String columnKey, ValueProvider<T, Date> valueProvider) {
		return addColumn(columnKey, new DateValueProvider<>(preferences.getDateFormat(), valueProvider));
	}

	/**
	 * Add a column that is mapped to a field of type {@link Date}. The field values will be converted by using the user's date time
	 * format.
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	public Column<T> addDateTimeColumn(String columnKey, ValueProvider<T, Date> valueProvider) {
		return addColumn(columnKey, new DateValueProvider<>(preferences.getDateTimeFormat(), valueProvider));
	}

	/**
	 * Add a column that is mapped to a field of type {@link LocalDate}. The field values will be converted by using the user's date
	 * format.
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	public Column<T> addLocalDateColumn(String columnKey, ValueProvider<T, LocalDate> valueProvider) {
		return addColumn(columnKey, new LocalDateValueProvider<>(preferences.getDateFormat(), valueProvider));
	}

	/**
	 * Add a column that is mapped to a field of type {@link LocalDateTime}. The field values will be converted by using the user's
	 * date time format.
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	public Column<T> addLocalDateTimeColumn(String columnKey, ValueProvider<T, LocalDateTime> valueProvider) {
		return addColumn(columnKey, new LocalDateTimeValueProvider<>(preferences.getDateTimeFormat(), valueProvider));
	}

	/**
	 * Add a column that is mapped to a field of type {@link GregorianCalendar}. The field values will be converted by using the
	 * user's date time format.
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	public Column<T> addGregorianCalendarColumn(String columnKey, ValueProvider<T, GregorianCalendar> valueProvider) {
		return addColumn(columnKey, new GregorianCalendarValueProvider<>(preferences.getDateTimeFormat(), valueProvider));
	}

	/**
	 * Add a column that is mapped to a field of type {@link Float}. The field values will be converted by using the user's number
	 * format.
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	public Column<T> addFloatColumn(String columnKey, ValueProvider<T, Float> valueProvider) {
		return addColumn(columnKey, new FloatValueProvider<>(preferences.getNumberFormat(), valueProvider));
	}

	/**
	 * Add a column that is mapped to a field of type {@link Double}. The field values will be converted by using the user's number
	 * format.
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	public Column<T> addDoubleColumn(String columnKey, ValueProvider<T, Double> valueProvider) {
		return addColumn(columnKey, new DoubleValueProvider<>(preferences.getNumberFormat(), valueProvider));
	}

	/**
	 * Add a column that is mapped to a field of type {@link Enum}. The field values will be converted by using the provided
	 * translation map.
	 * @param columnKey
	 * @param valueProvider
	 * @param translationMap
	 * @return the new column
	 */
	public Column<T> addEnumColumn(String columnKey, ValueProvider<T, Enum<?>> valueProvider, Map<String, String> translationMap) {
		return addColumn(columnKey, new EnumValueProvider<>(translationMap, valueProvider));
	}

	/**
	 * Navigate to the view of the given class
	 * @param viewClass
	 */
	public void navigateTo(Class<? extends Component> viewClass) {
		navigator.navigateTo(viewClass);
	}

	/**
	 * Navigate to the view of the given class
	 * @param id the ID that should be added to the route
	 * @param viewClass
	 */
	public void navigateTo(Class<? extends Component> viewClass, String id) {
		navigator.navigateTo(viewClass, id);
	}

	/**
	 * Navigate to the view of the given class
	 * @param id the ID that should be added to the route
	 * @param viewClass
	 */
	public void navigateTo(Class<? extends Component> viewClass, Long id) {
		navigator.navigateTo(viewClass, id);
	}

	/**
	 * Navigate to the view of the given class
	 * @param id the ID that should be added to the route
	 * @param viewClass
	 */
	public void navigateTo(Class<? extends Component> viewClass, Integer id) {
		navigator.navigateTo(viewClass, id);
	}

	/**
	 * Navigate to the view of the given class
	 * @param id the ID that should be added to the route
	 * @param viewClass
	 */
	public void navigateTo(Class<? extends Component> viewClass, UUID id) {
		navigator.navigateTo(viewClass, id);
	}

}
