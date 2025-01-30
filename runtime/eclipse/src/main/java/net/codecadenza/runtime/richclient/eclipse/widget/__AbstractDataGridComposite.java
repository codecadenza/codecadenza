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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import net.codecadenza.runtime.richclient.eclipse.widget.util.ColumnSorter;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.ITableFontProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

/**
 * <p>
 * Generic data grid composite
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the data grid
 */
public abstract class __AbstractDataGridComposite<T> extends Composite {
	private static final String EMPTY_CELL_CONTENT = "";

	protected TableViewer tableViewer;
	protected Table table;
	protected Menu popUpMenu;
	protected Shell parentShell;

	/**
	 * Constructor
	 * @param parent
	 */
	protected __AbstractDataGridComposite(Composite parent) {
		super(parent, SWT.NONE);
	}

	/**
	 * Get the cell background. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the cell background color
	 */
	@SuppressWarnings("unused")
	public Color getCellBackground(T element, int columnIndex) {
		return null;
	}

	/**
	 * Get the cell foreground color. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the cell foreground color
	 */
	@SuppressWarnings("unused")
	public Color getCellForeground(T element, int columnIndex) {
		return null;
	}

	/**
	 * Get the cell image. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the cell image
	 */
	@SuppressWarnings("unused")
	public Image getCellImage(T element, int columnIndex) {
		return null;
	}

	/**
	 * Get the cell text. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the cell text
	 */
	@SuppressWarnings("unused")
	public String getCellText(T element, int columnIndex) {
		return "";
	}

	/**
	 * Get the cell font. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the cell font
	 */
	@SuppressWarnings("unused")
	public Font getCellFont(T element, int columnIndex) {
		return null;
	}

	/**
	 * Callback method for double-click events
	 * @param element
	 */
	@SuppressWarnings("unused")
	public void onDoubleClick(T element) {

	}

	/**
	 * Show or hide the grid lines
	 * @param show
	 */
	public final void setLinesVisible(boolean show) {
		table.setLinesVisible(show);
	}

	/**
	 * Show or hide the grid header
	 * @param show
	 */
	public final void setHeaderVisible(boolean show) {
		table.setHeaderVisible(show);
	}

	/**
	 * Label Provider
	 */
	protected class TableLabelProvider extends LabelProvider
			implements ITableLabelProvider, ITableColorProvider, ITableFontProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableColorProvider#getBackground(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Color getBackground(Object element, int columnIndex) {
			final T domainObject = (T) element;
			return getCellBackground(domainObject, columnIndex);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableColorProvider#getForeground(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Color getForeground(Object element, int columnIndex) {
			final T domainObject = (T) element;
			return getCellForeground(domainObject, columnIndex);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Image getColumnImage(Object element, int columnIndex) {
			final T domainObject = (T) element;
			return getCellImage(domainObject, columnIndex);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public String getColumnText(Object element, int columnIndex) {
			final T domainObject = (T) element;
			return getCellText(domainObject, columnIndex);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableFontProvider#getFont(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Font getFont(Object element, int columnIndex) {
			final T domainObject = (T) element;
			return getCellFont(domainObject, columnIndex);
		}
	}

	/**
	 * Content provider
	 */
	protected class ContentProvider implements IStructuredContentProvider {
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
	 * Add a column without sorting capabilities
	 * @param name
	 * @param width
	 */
	public final void addColumn(String name, int width) {
		final var column = new TableColumn(table, SWT.NONE);
		column.setWidth(width);
		column.setText(name);
	}

	/**
	 * Add a column that can be sorted by using an alternative sort format
	 * @param name
	 * @param sortType
	 * @param sortFormat
	 * @param width
	 */
	public final void addColumn(String name, ColumnSortType sortType, String sortFormat, int width) {
		final var column = new TableColumn(table, SWT.NONE);
		column.setWidth(width);
		column.setText(name);

		// Add a column sorter
		column.addListener(SWT.Selection, new ColumnSorter(tableViewer, column, sortType, table.getColumnCount() - 1, sortFormat));
	}

	/**
	 * Add a column that can be sorted
	 * @param name
	 * @param sortType
	 * @param width
	 */
	public final void addColumn(String name, ColumnSortType sortType, int width) {
		addColumn(name, sortType, null, width);
	}

	/**
	 * Add a grid column
	 * @param name
	 */
	public final void addColumn(String name) {
		final var column = new TableColumn(table, SWT.NONE);
		column.setWidth(100);
		column.setText(name);
	}

	/**
	 * Set the data
	 * @param data
	 */
	public final void setData(Collection<T> data) {
		tableViewer.setInput(data);
	}

	/**
	 * Refresh the data grid
	 */
	public final void refresh() {
		tableViewer.refresh();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.widgets.Widget#getData()
	 */
	@Override
	@SuppressWarnings("unchecked")
	public Collection<T> getData() {
		return (Collection<T>) tableViewer.getInput();
	}

	/**
	 * @return the selected element
	 */
	@SuppressWarnings("unchecked")
	public final T getSelection() {
		final var s = (IStructuredSelection) tableViewer.getSelection();

		if (s == null)
			return null;

		if (s.getFirstElement() == null)
			return null;

		return (T) s.getFirstElement();
	}

	/**
	 * @return all selected elements
	 */
	@SuppressWarnings("unchecked")
	public final List<T> getAllSelectedElements() {
		final var selectionItems = new ArrayList<T>();
		final var s = (IStructuredSelection) tableViewer.getSelection();

		if (s == null)
			return Collections.emptyList();

		final List<?> sel = s.toList();

		sel.forEach(o -> selectionItems.add((T) o));

		return selectionItems;
	}

	/**
	 * @return the pop-up menu of the grid
	 */
	public final Menu getPopUpMenu() {
		return popUpMenu;
	}

	/**
	 * @return the content of the selected cell
	 */
	public String getSelectedCellContent() {
		return EMPTY_CELL_CONTENT;
	}

	/**
	 * @return the table viewer
	 */
	public final TableViewer getTableViewer() {
		return tableViewer;
	}

}
