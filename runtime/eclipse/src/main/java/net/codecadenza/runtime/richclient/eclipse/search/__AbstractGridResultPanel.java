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
package net.codecadenza.runtime.richclient.eclipse.search;

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_REFRESH;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_ACTION_MSG_FETCH_TITLE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_ACTION_MSG_QUERY_FAILED;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;

import java.lang.invoke.MethodHandles;
import java.text.DecimalFormat;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import net.codecadenza.runtime.richclient.eclipse.image.ImageCache;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.ITableFontProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract base class for displaying data in a grid
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the grid panel
 */
public abstract class __AbstractGridResultPanel<T> extends Composite {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	protected TableViewer tableViewer;
	protected Table table;
	protected ToolBar toolBar;
	protected FormatDTO userFormat;
	protected DecimalFormat decimalFormat;
	protected DateTimeFormatter dateTimeFormat;
	protected DateTimeFormatter dateFormat;
	protected Menu popUpMenu;

	/**
	 * Constructor
	 * @param parent
	 * @param style
	 */
	protected __AbstractGridResultPanel(final Composite parent, int style) {
		super(parent, style);

		setLayout(new GridLayout());

		userFormat = getFormatPreferences();
		decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
		dateTimeFormat = DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat()).withZone(ZoneId.systemDefault());
		dateFormat = DateTimeFormatter.ofPattern(userFormat.getDateFormat()).withZone(ZoneId.systemDefault());

		toolBar = new ToolBar(this, SWT.WRAP);
		toolBar.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var cmdRefresh = new ToolItem(toolBar, SWT.PUSH);
		cmdRefresh.setImage(ImageCache.getImage(ImageCache.IMG_REFRESH));
		cmdRefresh.setToolTipText(getTranslation(CMD_REFRESH));

		cmdRefresh.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				refreshData();
			}
		});

		tableViewer = new TableViewer(this, SWT.FULL_SELECTION | SWT.BORDER);
		tableViewer.setLabelProvider(new TableLabelProvider());
		tableViewer.setContentProvider(new ContentProvider());

		table = tableViewer.getTable();
		table.setLinesVisible(true);
		table.setHeaderVisible(true);
		table.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		popUpMenu = new Menu(table);
		table.setMenu(popUpMenu);

		initTableColumns(table);
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
	 * Label provider
	 */
	protected class TableLabelProvider extends LabelProvider
			implements ITableLabelProvider, ITableColorProvider, ITableFontProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Image getColumnImage(Object element, int columnIndex) {
			return getColImage((T) element, columnIndex);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public String getColumnText(Object element, int columnIndex) {
			return getColText((T) element, columnIndex);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableColorProvider#getBackground(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Color getBackground(Object element, int columnIndex) {
			return getCellBackground((T) element, columnIndex);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableColorProvider#getForeground(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Color getForeground(Object element, int columnIndex) {
			return getCellForeground((T) element, columnIndex);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableFontProvider#getFont(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Font getFont(Object element, int columnIndex) {
			return getCellFont((T) element, columnIndex);
		}
	}

	/**
	 * @return the pop-up menu of this grid
	 */
	public Menu getPopUpMenu() {
		return popUpMenu;
	}

	/**
	 * Initialize the table columns
	 * @param table
	 */
	public abstract void initTableColumns(Table table);

	/**
	 * Get the cell text
	 * @param element
	 * @param columnIndex
	 * @return the column text
	 */
	public abstract String getColText(T element, int columnIndex);

	/**
	 * @return the format preferences
	 */
	public abstract FormatDTO getFormatPreferences();

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
	 * Perform the data fetch operation
	 * @return the fetched data
	 */
	public abstract Collection<T> fetchData();

	/**
	 * @return the toolbar
	 */
	public ToolBar getToolBar() {
		return toolBar;
	}

	/**
	 * Get the column image. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the column image
	 */
	@SuppressWarnings("unused")
	public Image getColImage(T element, int columnIndex) {
		return null;
	}

	/**
	 * Refresh the data of the panel
	 */
	public void refreshData() {
		logger.debug("Refresh grid panel data");

		// Set the input
		try {
			tableViewer.setInput(fetchData());
		}
		catch (final Exception e) {
			logger.error("Error while refreshing grid panel data!", e);

			MessageDialog.openError(getShell(), getTranslation(DATA_FETCH_ACTION_MSG_FETCH_TITLE),
					getTranslation(DATA_FETCH_ACTION_MSG_QUERY_FAILED) + e.getMessage());
		}
	}

	/**
	 * @return the table viewer
	 */
	public TableViewer getTableViewer() {
		return tableViewer;
	}

	/**
	 * @return the selected element
	 */
	@SuppressWarnings("unchecked")
	public T getSelection() {
		final var s = (IStructuredSelection) tableViewer.getSelection();

		if (s == null)
			return null;

		if (s.getFirstElement() == null)
			return null;

		return (T) s.getFirstElement();
	}

}
