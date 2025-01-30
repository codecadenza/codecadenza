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
package net.codecadenza.runtime.richclient.eclipse.widget;

import java.util.Collection;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;

/**
 * <p>
 * Generic combobox widget
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the combobox
 */
public class DataComboViewer<T> extends Composite {
	private final ComboViewer comboViewer;

	/**
	 * Constructor
	 * @param parent
	 * @param style
	 */
	@SuppressWarnings("unchecked")
	public DataComboViewer(Composite parent, int style) {
		super(parent, SWT.None);

		this.setLayout(new FillLayout());

		comboViewer = new ComboViewer(this, style);
		comboViewer.setComparator(new ItemComparator());
		comboViewer.setContentProvider(new ContentProvider());
		comboViewer.setLabelProvider(new ListLabelProvider());

		comboViewer.addSelectionChangedListener(event -> {
			final var sel = (IStructuredSelection) event.getSelection();

			if (sel == null)
				onSelectionChanged(null);
			else
				onSelectionChanged((T) sel.getFirstElement());
		});
	}

	/**
	 * Callback listener that will be fired as soon as the selection has been changed
	 * @param element
	 */
	@SuppressWarnings("unused")
	public void onSelectionChanged(T element) {
		// The implementation must be provided by a subclass!
	}

	/**
	 * Get the image of the item. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @return the item image
	 */
	@SuppressWarnings("unused")
	public Image getItemImage(T element) {
		return null;
	}

	/**
	 * Get the item text. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @return the item text
	 */
	@SuppressWarnings("unused")
	public String getItemText(T element) {
		return "";
	}

	/**
	 * @return the viewer's instance
	 */
	public final ComboViewer getComboViewer() {
		return comboViewer;
	}

	/**
	 * @return the selected item
	 */
	@SuppressWarnings("unchecked")
	public final T getSelectedItem() {
		final var sel = (IStructuredSelection) comboViewer.getSelection();

		if (sel == null)
			return null;

		return (T) sel.getFirstElement();
	}

	/**
	 * Set the selected item
	 * @param item
	 */
	public final void setSelectedItem(T item) {
		comboViewer.setSelection(new StructuredSelection(item), true);
	}

	/**
	 * Set the data
	 * @param data
	 */
	public final void setData(Collection<T> data) {
		comboViewer.setInput(data);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.widgets.Control#setToolTipText(java.lang.String)
	 */
	@Override
	public void setToolTipText(String toolTipText) {
		comboViewer.getControl().setToolTipText(toolTipText);
	}

	/**
	 * Content provider
	 */
	private class ContentProvider implements IStructuredContentProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Object[] getElements(Object inputElement) {
			return ((Collection<T>) inputElement).toArray();
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
		 */
		@Override
		public void dispose() {
			// No implementation required!
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			// No implementation required!
		}
	}

	/**
	 * Compare items
	 * @param item1
	 * @param item2
	 * @return the result of the comparison
	 */
	public int compareItems(T item1, T item2) {
		return getItemText(item1).compareTo(getItemText(item2));
	}

	/**
	 * Comparator that compares two items by using the method compareItems()
	 */
	class ItemComparator extends ViewerComparator {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ViewerComparator#compare(org.eclipse.jface.viewers.Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public int compare(Viewer viewer, Object e1, Object e2) {
			final T item1 = (T) e1;
			final T item2 = (T) e2;

			return compareItems(item1, item2);
		}
	}

	/**
	 * Label provider
	 */
	private class ListLabelProvider extends LabelProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public String getText(Object element) {
			final T domainObject = (T) element;

			return getItemText(domainObject);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Image getImage(Object element) {
			final T domainObject = (T) element;

			return getItemImage(domainObject);
		}
	}

}
