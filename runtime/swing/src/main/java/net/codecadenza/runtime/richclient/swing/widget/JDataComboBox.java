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
package net.codecadenza.runtime.richclient.swing.widget;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import javax.swing.AbstractListModel;
import javax.swing.ComboBoxModel;
import javax.swing.JComboBox;

/**
 * <p>
 * Enhanced combobox with a custom data model
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the combobox
 */
public class JDataComboBox<T> extends JComboBox<String> {
	private static final long serialVersionUID = -7629649405213109720L;

	private DataComboBoxModel model;

	/**
	 * Constructor
	 */
	public JDataComboBox() {
		this.addActionListener(_ -> {
			final T selObject = JDataComboBox.this.getSelectedModelObject();
			onSelectionChanged(selObject);
		});
	}

	/**
	 * Constructor
	 * @param modelElements
	 */
	public JDataComboBox(List<T> modelElements) {
		this();

		setData(modelElements);
	}

	/**
	 * @param element
	 * @return the text to be displayed
	 */
	public String getItemText(T element) {
		return element.toString();
	}

	/**
	 * Data model
	 */
	private class DataComboBoxModel extends AbstractListModel<String> implements ComboBoxModel<String> {
		private static final long serialVersionUID = 5333857556644020844L;
		private transient List<T> elements = new ArrayList<>();
		private final HashMap<String, T> indexMap = new HashMap<>();
		private transient T selection;
		private String selectionString;

		/**
		 * Constructor
		 * @param modelElements a list containing all elements to be displayed
		 */
		public DataComboBoxModel(List<T> modelElements) {
			elements = modelElements;

			// Save the element in the map in order to find the model object afterwards
			elements.forEach(element -> indexMap.put(getItemText(element), element));
		}

		/*
		 * (non-Javadoc)
		 * @see javax.swing.ListModel#getElementAt(int)
		 */
		@Override
		public String getElementAt(int index) {
			return getItemText(elements.get(index));
		}

		/*
		 * (non-Javadoc)
		 * @see javax.swing.ListModel#getSize()
		 */
		@Override
		public int getSize() {
			return elements.size();
		}

		/*
		 * (non-Javadoc)
		 * @see javax.swing.ComboBoxModel#setSelectedItem(java.lang.Object)
		 */
		@Override
		public void setSelectedItem(Object anItem) {
			selectionString = (String) anItem;
			selection = null;

			if (selectionString == null)
				return;

			if (indexMap.containsKey(selectionString))
				selection = indexMap.get(selectionString);
		}

		/*
		 * (non-Javadoc)
		 * @see javax.swing.ComboBoxModel#getSelectedItem()
		 */
		@Override
		public Object getSelectedItem() {
			return selectionString;
		}

		/**
		 * @return the selected model object
		 */
		public T getSelectedObject() {
			return selection;
		}
	}

	/**
	 * Set the list containing all elements of the data model
	 * @param modelElements
	 */
	public void setData(List<T> modelElements) {
		model = new DataComboBoxModel(modelElements);
		setModel(model);
	}

	/**
	 * Set the collection containing all elements of the data model
	 * @param modelElements
	 */
	public void setData(Collection<T> modelElements) {
		final var list = new ArrayList<>(modelElements);

		setData(list);
	}

	/**
	 * @return the selected model object
	 */
	public T getSelectedModelObject() {
		if (model == null)
			return null;

		return model.getSelectedObject();
	}

	/**
	 * Set the selected model object
	 * @param selection
	 */
	public void setSelectedModelObject(T selection) {
		if (model == null)
			return;

		model.setSelectedItem(getItemText(selection));
	}

	/**
	 * Event that will be fired as soon as the selection has been changed
	 * @param selection
	 */
	@SuppressWarnings("unused")
	public void onSelectionChanged(T selection) {
		// The implementation must be provided by a subclass!
	}

}
