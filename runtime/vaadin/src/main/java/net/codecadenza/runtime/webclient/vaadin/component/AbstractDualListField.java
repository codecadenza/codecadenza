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

import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.DUAL_LIST_FIELD_LEFT_COL;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.DUAL_LIST_FIELD_RIGHT_COL;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.HasValue;
import com.vaadin.flow.component.HasValue.ValueChangeEvent;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.Unit;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.Grid.SelectionMode;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.provider.AbstractBackEndDataProvider;
import com.vaadin.flow.function.ValueProvider;
import com.vaadin.flow.shared.Registration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;
import net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService;

/**
 * <p>
 * Abstract base class for selecting items from a list
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type that is used by this component
 */
@Tag("div")
public abstract class AbstractDualListField<T> extends Component
		implements HasValue<ValueChangeEvent<Collection<T>>, Collection<T>>, HasSize {
	private static final long serialVersionUID = -8734486613785199838L;
	private static final String ATTRIBUTE_LIST_TYPE = "list-type";

	protected final InternalI18NService i18n;
	protected final VerticalLayout vlContent = new VerticalLayout();
	protected final HorizontalLayout hlSelection = new HorizontalLayout();
	protected final Button cmdAddSelected = new Button();
	protected final Button cmdRemoveSelected = new Button();
	protected final Grid<T> gridSource = new Grid<>();
	protected final Grid<T> gridTarget = new Grid<>();
	protected ValueProvider<T, ?> valueProvider;
	protected transient Collection<T> selectedItems = new ArrayList<>();
	protected boolean readOnly;

	/**
	 * Constructor
	 * @param locale
	 */
	protected AbstractDualListField(Locale locale) {
		this.i18n = new InternalI18NService(locale);
	}

	/**
	 * @return the data provider of this component
	 */
	public abstract AbstractBackEndDataProvider<T, String> getDataProvider();

	/**
	 * @param valueProvider
	 */
	public void setItemLabelGenerator(ValueProvider<T, ?> valueProvider) {
		this.valueProvider = valueProvider;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.Component#onAttach(com.vaadin.flow.component.AttachEvent)
	 */
	@Override
	protected void onAttach(AttachEvent attachEvent) {
		gridSource.setSizeFull();
		gridSource.setSelectionMode(SelectionMode.MULTI);
		gridSource.addThemeVariants(GridVariant.LUMO_COMPACT, GridVariant.LUMO_ROW_STRIPES);
		gridSource.addColumn(valueProvider).setSortable(true).setHeader(i18n.getTranslation(DUAL_LIST_FIELD_LEFT_COL));
		gridSource.setDataProvider(getDataProvider());
		gridSource.getElement().setAttribute(ATTRIBUTE_LIST_TYPE, "source");

		gridSource.addItemDoubleClickListener(event -> {
			if (readOnly || selectedItems.contains(event.getItem()))
				return;

			selectedItems.add(event.getItem());
			gridTarget.setItems(selectedItems);
		});

		gridTarget.setSizeFull();
		gridTarget.setSelectionMode(SelectionMode.MULTI);
		gridTarget.addThemeVariants(GridVariant.LUMO_COMPACT, GridVariant.LUMO_ROW_STRIPES);
		gridTarget.addColumn(valueProvider).setSortable(true).setHeader(i18n.getTranslation(DUAL_LIST_FIELD_RIGHT_COL));
		gridTarget.getElement().setAttribute(ATTRIBUTE_LIST_TYPE, "target");

		gridTarget.addItemDoubleClickListener(event -> {
			if (readOnly)
				return;

			selectedItems.remove(event.getItem());
			gridTarget.setItems(selectedItems);
		});

		cmdAddSelected.setIcon(new Icon(VaadinIcon.ARROW_RIGHT));

		cmdAddSelected.addClickListener(_ -> {
			if (readOnly)
				return;

			for (final var selItem : gridSource.getSelectedItems())
				if (!selectedItems.contains(selItem))
					selectedItems.add(selItem);

			gridTarget.setItems(selectedItems);
		});

		cmdRemoveSelected.setIcon(new Icon(VaadinIcon.ARROW_LEFT));

		cmdRemoveSelected.addClickListener(_ -> {
			if (readOnly)
				return;

			selectedItems.removeAll(gridTarget.getSelectedItems());

			gridTarget.setItems(selectedItems);
		});

		final var vlButtons = new VerticalLayout();
		vlButtons.setWidth(70, Unit.PIXELS);
		vlButtons.addAndExpand(new VerticalLayout());
		vlButtons.add(cmdAddSelected, cmdRemoveSelected);
		vlButtons.addAndExpand(new VerticalLayout());

		hlSelection.add(gridSource, vlButtons, gridTarget);
		hlSelection.setSpacing(false);
		hlSelection.setPadding(false);
		hlSelection.setSizeFull();

		vlContent.setSizeFull();
		vlContent.setPadding(false);
		vlContent.add(hlSelection);

		getElement().appendChild(vlContent.getElement());
	}

	/**
	 * @param enabled
	 */
	public void setEnabled(boolean enabled) {
		cmdAddSelected.setEnabled(enabled);
		cmdRemoveSelected.setEnabled(enabled);
		gridSource.setEnabled(enabled);
		gridTarget.setEnabled(enabled);
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#getValue()
	 */
	@Override
	public Collection<T> getValue() {
		return selectedItems;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#setValue(java.lang.Object)
	 */
	@Override
	public void setValue(Collection<T> value) {
		selectedItems = value;

		gridTarget.setItems(value);
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#addValueChangeListener(com.vaadin.flow.component.HasValue.ValueChangeListener)
	 */
	@Override
	public Registration addValueChangeListener(ValueChangeListener<? super ValueChangeEvent<Collection<T>>> valueChangeListener) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#isReadOnly()
	 */
	@Override
	public boolean isReadOnly() {
		return readOnly;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#setReadOnly(boolean)
	 */
	@Override
	public void setReadOnly(boolean readOnly) {
		this.readOnly = readOnly;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#isRequiredIndicatorVisible()
	 */
	@Override
	public boolean isRequiredIndicatorVisible() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#setRequiredIndicatorVisible(boolean)
	 */
	@Override
	public void setRequiredIndicatorVisible(boolean requiredIndicatorVisible) {
		// No implementation required!
	}

}
