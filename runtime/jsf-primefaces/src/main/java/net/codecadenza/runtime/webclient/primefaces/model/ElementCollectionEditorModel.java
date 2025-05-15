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
	private List<RowElement> rowElements;
	private Collection<T> elements;
	private String newElement;
	private RowElement selectedElement;

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
	public RowElement getSelectedRowElement() {
		return selectedElement;
	}

	/**
	 * Set the selected row element
	 * @param selectedElement
	 */
	public void setSelectedRowElement(RowElement selectedElement) {
		this.selectedElement = selectedElement;
	}

	/**
	 * Callback method that keeps track of the selected row element
	 * @param event
	 */
	public void onRowSelect(SelectEvent<RowElement> event) {
		setSelectedRowElement(event.getObject());
	}

	/**
	 * Set the elements
	 * @param elements
	 */
	public void setElements(Collection<T> elements) {
		this.elements = elements;

		refreshElementsToBeDisplayed();
	}

	/**
	 * @return the elements
	 */
	public Collection<T> getElements() {
		return elements;
	}

	/**
	 * @return all elements
	 */
	public Collection<RowElement> getRowElements() {
		return rowElements;
	}

	/**
	 * Determine the elements to be displayed by using the value in the 'newElement' field as filter
	 */
	public void refreshElementsToBeDisplayed() {
		if (newElement == null || newElement.isEmpty() || newElement.equals(valueConverter.getInitialDefaultValue()))
			rowElements = elements.stream().sorted().map(valueConverter::convertToString).map(RowElement::new)
					.collect(Collectors.toCollection(ArrayList::new));
		else
			rowElements = elements.stream().sorted().map(valueConverter::convertToString).filter(item -> item.startsWith(newElement))
					.map(RowElement::new).collect(Collectors.toCollection(ArrayList::new));
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

		elements.add(elementValue);

		// Reset the value to display all elements after adding a new one
		newElement = null;

		refreshElementsToBeDisplayed();
	}

	/**
	 * Delete the selected element
	 */
	public void deleteElement() {
		if (selectedElement != null) {
			elements.remove(valueConverter.convertToValue(selectedElement.getValue()));
			refreshElementsToBeDisplayed();
		}
	}

	/**
	 * Delete all elements
	 */
	public void deleteAll() {
		elements.clear();
		refreshElementsToBeDisplayed();
	}

}
