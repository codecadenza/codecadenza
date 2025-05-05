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
package net.codecadenza.runtime.webclient.primefaces.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import net.codecadenza.runtime.conversion.ValueConverter;
import org.primefaces.event.SelectEvent;

/**
 * <p>
 * Model for element collection editors
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of an element in the collection
 */
public class ElementCollectionEditorModel<T> {
	private final ValueConverter<T> valueConverter;
	private List<RowElement<T>> rowElements;
	private Collection<T> elements;
	private String newElement;
	private RowElement<T> selectedElement;

	/**
	 * Constructor
	 * @param decimalFormat
	 * @param dateTimeFormat
	 * @param dateFormat
	 * @param elementType
	 */
	public ElementCollectionEditorModel(String decimalFormat, String dateTimeFormat, String dateFormat, Class<T> elementType) {
		this.valueConverter = new ValueConverter<>(decimalFormat, dateTimeFormat, dateFormat, elementType);
		this.newElement = valueConverter.getInitialDefaultValue();
	}

	/**
	 * @return the new element as a string
	 */
	public String getNewElement() {
		return newElement;
	}

	/**
	 * Set the new element
	 * @param newElement
	 */
	public void setNewElement(String newElement) {
		this.newElement = newElement;
	}

	/**
	 * @return the selected row element
	 */
	public RowElement<T> getSelectedRowElement() {
		return selectedElement;
	}

	/**
	 * Set the selected row element
	 * @param selectedElement
	 */
	public void setSelectedRowElement(RowElement<T> selectedElement) {
		this.selectedElement = selectedElement;
	}

	/**
	 * Callback method that keeps track of the selected row element
	 * @param event
	 */
	public void onRowSelect(SelectEvent<RowElement<T>> event) {
		setSelectedRowElement(event.getObject());
	}

	/**
	 * Set the elements
	 * @param elements
	 */
	public void setElements(Collection<T> elements) {
		this.elements = elements;

		this.rowElements = elements.stream().map(RowElement::new).collect(Collectors.toCollection(ArrayList::new));
	}

	/**
	 * @return the elements
	 */
	public Collection<T> getElements() {
		return this.elements;
	}

	/**
	 * @return all elements
	 */
	public Collection<RowElement<T>> getRowElements() {
		return rowElements;
	}

	/**
	 * Add a new element
	 */
	public void addElement() {
		if (newElement == null)
			throw new IllegalArgumentException("The new element must not be null!");

		if (newElement.isEmpty())
			throw new IllegalArgumentException("The new element must not be empty!");

		final T elementValue = valueConverter.convertToValue(newElement);

		rowElements.add(new RowElement<>(elementValue));
		elements.add(elementValue);
	}

	/**
	 * Convert the given row to its string representation
	 * @param element
	 * @return the string representation of the given row
	 */
	public String convertToString(RowElement<T> element) {
		return valueConverter.convertToString(element.getValue());
	}

	/**
	 * Delete the selected element
	 */
	public void deleteElement() {
		if (selectedElement != null) {
			rowElements.remove(selectedElement);
			elements.remove(selectedElement.getValue());
		}
	}

	/**
	 * Delete all elements
	 */
	public void deleteAll() {
		rowElements.clear();
		elements.clear();
	}

}
