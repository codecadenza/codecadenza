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

import com.vaadin.flow.component.HasValue;
import com.vaadin.flow.component.HasValue.ValueChangeEvent;
import com.vaadin.flow.shared.Registration;
import java.util.Set;
import java.util.stream.Collectors;
import net.codecadenza.runtime.webclient.vaadin.i18n.I18NService;
import net.codecadenza.runtime.webclient.vaadin.util.PreferencesStore;

/**
 * <p>
 * Editor for maintaining element collections that are backed by a {@link Set}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of an element in the collection
 */
public class ElementCollectionSetEditor<T> extends AbstractElementCollectionEditor<T>
		implements HasValue<ValueChangeEvent<Set<T>>, Set<T>> {
	private static final long serialVersionUID = 5872529094974086967L;

	/**
	 * Constructor
	 * @param readOnly
	 * @param elementType
	 * @param i18n
	 * @param preferences
	 */
	public ElementCollectionSetEditor(boolean readOnly, Class<T> elementType, I18NService i18n, PreferencesStore preferences) {
		super(readOnly, elementType, i18n, preferences);
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#getValue()
	 */
	@Override
	public Set<T> getValue() {
		return elements.stream().collect(Collectors.toSet());
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#setValue(java.lang.Object)
	 */
	@Override
	public void setValue(Set<T> value) {
		elements = value;

		grid.setItems(value);
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#addValueChangeListener(com.vaadin.flow.component.HasValue.ValueChangeListener)
	 */
	@Override
	public Registration addValueChangeListener(ValueChangeListener<? super ValueChangeEvent<Set<T>>> valueChangeListener) {
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
