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

import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.LOV_FIELD_CBOITEMLIST_INPUTPROMPT;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.HasValue;
import com.vaadin.flow.component.HasValue.ValueChangeEvent;
import com.vaadin.flow.component.ItemLabelGenerator;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.shared.Registration;
import java.util.Locale;
import net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService;

/**
 * <p>
 * This class represents a composite component that provides the selection of an item by opening a separate list-of-values dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the LOV field
 */
@Tag("div")
public class ListOfValuesField<T> extends Component implements HasValue<ValueChangeEvent<T>, T>, HasSize {
	private static final long serialVersionUID = -8734486613785199838L;

	protected final TextField txtValue = new TextField();
	protected final Button cmdOpen = new Button();
	protected final InternalI18NService i18n;
	protected ItemLabelGenerator<T> itemLabelGenerator;
	private transient T value;

	/**
	 * Constructor
	 * @param clickListener a callback that is fired as soon as a user clicks on the link to open a LOV
	 * @param locale
	 */
	public ListOfValuesField(ComponentEventListener<ClickEvent<Button>> clickListener, Locale locale) {
		this.i18n = new InternalI18NService(locale);

		if (clickListener != null)
			this.cmdOpen.addClickListener(clickListener);
	}

	/**
	 * Constructor for creating a read-only LOV field
	 * @param locale
	 */
	public ListOfValuesField(Locale locale) {
		this(null, locale);

		setReadOnly(true);
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.Component#onAttach(com.vaadin.flow.component.AttachEvent)
	 */
	@Override
	protected void onAttach(AttachEvent attachEvent) {
		txtValue.setWidthFull();
		txtValue.setPlaceholder(i18n.getTranslation(LOV_FIELD_CBOITEMLIST_INPUTPROMPT));

		cmdOpen.setIcon(new Icon(VaadinIcon.LIST));

		final var hlContent = new HorizontalLayout();
		hlContent.setMargin(false);
		hlContent.add(txtValue, cmdOpen);
		hlContent.setAlignSelf(FlexComponent.Alignment.END, cmdOpen);

		getElement().appendChild(hlContent.getElement());
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#getValue()
	 */
	@Override
	public T getValue() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#setValue(java.lang.Object)
	 */
	@Override
	public void setValue(T value) {
		this.value = value;

		if (value != null && itemLabelGenerator != null)
			txtValue.setValue(itemLabelGenerator.apply(value));
		else
			txtValue.setValue("");
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#addValueChangeListener(com.vaadin.flow.component.HasValue.ValueChangeListener)
	 */
	@Override
	public Registration addValueChangeListener(ValueChangeListener<? super ValueChangeEvent<T>> valueChangeListener) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#isReadOnly()
	 */
	@Override
	public boolean isReadOnly() {
		return txtValue.isReadOnly();
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#setReadOnly(boolean)
	 */
	@Override
	public void setReadOnly(boolean readOnly) {
		txtValue.setReadOnly(readOnly);
		cmdOpen.setEnabled(!readOnly);
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#isRequiredIndicatorVisible()
	 */
	@Override
	public boolean isRequiredIndicatorVisible() {
		return txtValue.isRequiredIndicatorVisible();
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#setRequiredIndicatorVisible(boolean)
	 */
	@Override
	public void setRequiredIndicatorVisible(boolean requiredIndicatorVisible) {
		txtValue.setRequiredIndicatorVisible(requiredIndicatorVisible);
	}

	/**
	 * @return the title
	 */
	public String getTitle() {
		return txtValue.getTitle();
	}

	/**
	 * @param title
	 */
	public void setTitle(String title) {
		txtValue.setTitle(title);
	}

	/**
	 * @param itemLabelGenerator
	 */
	public void setItemLabelGenerator(ItemLabelGenerator<T> itemLabelGenerator) {
		this.itemLabelGenerator = itemLabelGenerator;
	}

	/**
	 * @param label
	 */
	public void setLabel(String label) {
		txtValue.setLabel(label);
	}

	/**
	 * @return the label
	 */
	public String getLabel() {
		return txtValue.getLabel();
	}

	/**
	 * @param enabled
	 */
	public void setEnabled(boolean enabled) {
		txtValue.setEnabled(enabled);
		cmdOpen.setEnabled(enabled);
	}

}
