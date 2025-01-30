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

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_DATA_TABLE_PANEL_MSG_DEFAULT;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_DATA_TABLE_PANEL_MSG_FETCH_FAILED;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_DATA_TABLE_PANEL_MSG_RESULT;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_DATA_TABLE_PANEL_STATUS_FETCH_DATA;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Font;
import java.awt.SystemColor;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.text.DecimalFormat;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.SwingWorker;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;
import net.codecadenza.runtime.richclient.swing.widget.JDataTable.DeleteKeyPressedListener;
import net.codecadenza.runtime.richclient.swing.widget.JDataTable.EnterKeyPressedListener;
import net.codecadenza.runtime.richclient.swing.widget.JDataTable.RefreshKeyPressedListener;
import net.codecadenza.runtime.richclient.swing.widget.JDataTable.SortKeyAccessor;
import net.codecadenza.runtime.richclient.swing.widget.JDataTable.TableDoubleClickListener;
import net.codecadenza.runtime.richclient.swing.widget.actions.CopyCellToClipboardAction;
import net.codecadenza.runtime.richclient.swing.widget.actions.CopyColumnToClipboardAction;
import net.codecadenza.runtime.richclient.swing.widget.actions.CopyRowToClipboardAction;
import net.codecadenza.runtime.richclient.swing.widget.actions.ExportXLSAction;
import net.codecadenza.runtime.richclient.swing.widget.actions.RefreshAction;
import org.jdesktop.swingx.JXTable;
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
 * @param <T> the type of the data table
 */
public abstract class AbstractDataTablePanel<T> extends JPanel implements TableDoubleClickListener<T>,
		DeleteKeyPressedListener<T>, EnterKeyPressedListener<T>, RefreshKeyPressedListener, StatusReceivable {
	private static final long serialVersionUID = -1571491213988366173L;

	protected final JDataTable<T> table;
	protected JStatusPanel statusPanel;
	protected JToolBar toolBar;
	protected JPopupMenu popupMenu;
	protected RefreshAction refreshAction;
	protected CopyCellToClipboardAction cellCopyAction;
	protected CopyRowToClipboardAction rowCopyAction;
	protected CopyColumnToClipboardAction columnCopyAction;
	protected ExportXLSAction xlsExportAction;
	protected FormatDTO userFormat = FormatPreferencesManager.getFormatDTO();
	protected transient DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(userFormat.getDateFormat())
			.withZone(ZoneId.systemDefault());
	protected transient DateTimeFormatter dateTimeFormat = DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat())
			.withZone(ZoneId.systemDefault());
	protected DecimalFormat decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
	protected boolean readonly;

	/**
	 * Operation to get data
	 * @return a list containing data
	 */
	public abstract Collection<T> fetchData();

	/**
	 * @return the column definition list
	 */
	public abstract List<ColumnInfo> getColumnDefinition();

	/**
	 * Every non-abstract subclass must provide a logger instance!
	 * @return the logger
	 */
	protected abstract Logger getLogger();

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
	public Icon getCellImage(T element, int columnIndex) {
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
	 * Get the cell's value for sorting. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the comparable value
	 */
	@SuppressWarnings("unused")
	public Comparable<?> getCellValue(T element, int columnIndex) {
		return null;
	}

	/**
	 * @param element
	 * @param columnIndex
	 * @return the string interpretation of a cell to be exported
	 */
	public String getCellExportText(T element, int columnIndex) {
		return getCellText(element, columnIndex);
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
	 * @return a list containing all selected elements
	 */
	public final List<T> getAllSelectedElements() {
		return table.getSelectedElements();
	}

	/**
	 * @param columnIndex
	 * @param element
	 * @return the comparable property of a cell
	 */
	public Comparable<?> getComparableProperty(int columnIndex, T element) {
		return getCellValue(element, columnIndex);
	}

	/**
	 * @return the currently selected element
	 */
	public T getSelectedElement() {
		return table.getSelectedElement();
	}

	/**
	 * Remove the given element from the model
	 * @param element
	 */
	public void removeElementFromModel(T element) {
		table.removeElementFromModel(element);
	}

	/**
	 * Perform a data fetch operation without blocking the main thread
	 */
	public final void performFetch() {
		getLogger().debug("Perform data fetch operation");

		setCursor(new Cursor(Cursor.WAIT_CURSOR));

		statusPanel.setBusy(true);
		statusPanel.setMessage(getTranslation(ABSTRACT_DATA_TABLE_PANEL_STATUS_FETCH_DATA));

		final long start = System.currentTimeMillis();

		new SwingWorker<Collection<T>, Void>() {
			/*
			 * (non-Javadoc)
			 * @see javax.swing.SwingWorker#doInBackground()
			 */
			@Override
			protected Collection<T> doInBackground() throws Exception {
				return fetchData();
			}

			/*
			 * (non-Javadoc)
			 * @see javax.swing.SwingWorker#done()
			 */
			@Override
			protected void done() {
				try {
					final Collection<T> data = get();

					table.setData(data);

					final long end = System.currentTimeMillis();

					final var params = new ArrayList<>();
					params.add(data.size());
					params.add(String.format("%.2f", (double) (end - start) / 1000));

					statusPanel.setMessage(getTranslation(ABSTRACT_DATA_TABLE_PANEL_MSG_RESULT, params.toArray()));

					getLogger().debug("Data fetch operation finished");
				}
				catch (final InterruptedException e) {
					Thread.currentThread().interrupt();
					getLogger().error("Data fetch operation has been interrupted!", e);

					statusPanel.setMessage(getTranslation(ABSTRACT_DATA_TABLE_PANEL_MSG_FETCH_FAILED) + e.getMessage());
				}
				catch (final Exception e) {
					getLogger().error("Error while fetching data!", e);

					statusPanel.setMessage(getTranslation(ABSTRACT_DATA_TABLE_PANEL_MSG_FETCH_FAILED) + e.getMessage());
				}
				finally {
					statusPanel.setBusy(false);
					setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				}
			}
		}.execute();
	}

	/**
	 * Define if a user may select multiple rows or not!
	 * @param mode
	 */
	public void enableMultiSelectionMode(boolean mode) {
		table.enableMultiSelectionMode(mode);
	}

	/**
	 * @param renderer
	 * @param sortKeyAccessor
	 * @return the data table
	 */
	@SuppressWarnings("unused")
	protected JDataTable<T> initTable(AbstractDataTableRenderer<T> renderer, SortKeyAccessor<T> sortKeyAccessor) {
		return new JDataTable<>(renderer, createSortKeyAccessor());
	}

	/**
	 * @return the table component of this panel
	 */
	public JDataTable<T> getTable() {
		return table;
	}

	/**
	 * Initialize actions
	 */
	protected void initActions() {
		refreshAction = new RefreshAction(this);
		cellCopyAction = new CopyCellToClipboardAction(table);
		rowCopyAction = new CopyRowToClipboardAction(table);
		columnCopyAction = new CopyColumnToClipboardAction(table);
		xlsExportAction = new ExportXLSAction(this);
	}

	/**
	 * Initialize the toolbar
	 */
	protected void initToolBar() {
		toolBar.add(refreshAction);
		toolBar.add(xlsExportAction);
	}

	/**
	 * Initialize the pop-up menu
	 */
	protected void initPopUpMenu() {
		final var menuItemRefresh = new JMenuItem();
		menuItemRefresh.setAction(refreshAction);

		popupMenu.add(menuItemRefresh);

		final var menuItemExport = new JMenuItem();
		menuItemExport.setAction(xlsExportAction);

		popupMenu.add(menuItemExport);

		final var menuItemCopyCell = new JMenuItem();
		menuItemCopyCell.setAction(cellCopyAction);

		popupMenu.add(menuItemCopyCell);

		final var menuItemCopyRow = new JMenuItem();
		menuItemCopyRow.setAction(rowCopyAction);

		popupMenu.add(menuItemCopyRow);

		final var menuItemCopyCol = new JMenuItem();
		menuItemCopyCol.setAction(columnCopyAction);

		popupMenu.add(menuItemCopyCol);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.StatusReceivable#setStatusErrorMessage(java.lang.String)
	 */
	@Override
	public void setStatusErrorMessage(String err) {
		statusPanel.setMessage(err);
		statusPanel.setMessageIcon(ImageLoader.getImage(ImageLoader.ERROR));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.StatusReceivable#setStatusInfoMessage(java.lang.String)
	 */
	@Override
	public void setStatusInfoMessage(String info) {
		statusPanel.setMessage(info);
		statusPanel.setMessageIcon(ImageLoader.getImage(ImageLoader.INFO));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.StatusReceivable#setBusy(boolean)
	 */
	@Override
	public void setBusy(boolean busy) {
		statusPanel.setBusy(busy);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.StatusReceivable#setStatusIcon(javax.swing.ImageIcon)
	 */
	@Override
	public void setStatusIcon(ImageIcon statusIcon) {
		statusPanel.setStatusIcon(statusIcon);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.JDataTable.RefreshKeyPressedListener#onRefreshKeyPressed()
	 */
	@Override
	public void onRefreshKeyPressed() {
		refreshAction.actionPerformed(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.JDataTable.TableDoubleClickListener#onDblClick(java.lang.Object)
	 */
	@Override
	public void onDblClick(T selectedElement) {
		// A subclass can override the empty default implementation!
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.JDataTable.DeleteKeyPressedListener#
	 * onDeleteKeyPressed(java.lang.Object)
	 */
	@Override
	public void onDeleteKeyPressed(T selectedElement) {
		// A subclass can override the empty default implementation!
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.JDataTable.EnterKeyPressedListener# onEnterKeyPressed(java.lang.Object)
	 */
	@Override
	public void onEnterKeyPressed(T selectedElement) {
		// A subclass can override the empty default implementation!
	}

	/**
	 * Constructor
	 * @param allowMultiSelection
	 * @param readonly
	 */
	protected AbstractDataTablePanel(boolean allowMultiSelection, boolean readonly) {
		this.readonly = readonly;

		getLogger().debug("Initialize data table panel");

		// Create a renderer and delegate its functionality back to the panel!
		final var renderer = new AbstractDataTableRenderer<T>() {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer#getCellText(java.lang.Object, int)
			 */
			@Override
			protected String getCellText(T value, int column) {
				return AbstractDataTablePanel.this.getCellText(value, column);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer#getCellImage(java.lang.Object, int)
			 */
			@Override
			protected Icon getCellImage(T value, int column) {
				return AbstractDataTablePanel.this.getCellImage(value, column);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer#getCellFont(java.lang.Object, int)
			 */
			@Override
			protected Font getCellFont(T value, int column) {
				return AbstractDataTablePanel.this.getCellFont(value, column);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer#getCellForeground(java.lang.Object, int)
			 */
			@Override
			protected Color getCellForeground(T value, int column) {
				return AbstractDataTablePanel.this.getCellForeground(value, column);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer#getCellBackground(java.lang.Object, int)
			 */
			@Override
			protected Color getCellBackground(T value, int column) {
				return AbstractDataTablePanel.this.getCellBackground(value, column);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer#getCellValue(java.lang.Object, int)
			 */
			@Override
			protected Comparable<?> getCellValue(T value, int column) {
				return AbstractDataTablePanel.this.getCellValue(value, column);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer#getCellExportText(java.lang.Object, int)
			 */
			@Override
			protected String getCellExportText(T element, int columnIndex) {
				final String text = AbstractDataTablePanel.this.getCellExportText(element, columnIndex);
				return text == null ? "" : text;
			}
		};

		table = initTable(renderer, createSortKeyAccessor());
		table.registerDoubleClickListener(this);
		table.registerDeleteKeyPressedListener(this);
		table.registerEnterKeyPressedListener(this);
		table.registerRefreshKeyPressedListener(this);
		table.enableMultiSelectionMode(allowMultiSelection);

		this.setLayout(new BorderLayout(0, 2));

		toolBar = new JToolBar();
		toolBar.setFloatable(false);

		this.add(toolBar, BorderLayout.NORTH);

		popupMenu = new JPopupMenu();

		table.setColumnSelectionAllowed(false);
		table.setRowSelectionAllowed(true);
		table.setAutoResizeMode(JXTable.AUTO_RESIZE_OFF);
		table.getTableHeader().setReorderingAllowed(true);
		table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		table.setGridColor(SystemColor.lightGray);
		table.addMouseListener(new MouseListener(popupMenu));

		// Create actions
		initActions();

		// Initialize the toolbar
		initToolBar();

		// Initialize the pop-up menu
		initPopUpMenu();

		final var scrollPane = new JScrollPane();
		scrollPane.setViewportView(table);

		this.add(scrollPane, BorderLayout.CENTER);

		table.initColumns(getColumnDefinition());

		statusPanel = new JStatusPanel();
		statusPanel.setMessage(getTranslation(ABSTRACT_DATA_TABLE_PANEL_MSG_DEFAULT));

		this.add(statusPanel, BorderLayout.SOUTH);

		getLogger().debug("Data table panel initialization finished");
	}

	/**
	 * @return the panel's toolbar
	 */
	public JToolBar getToolbar() {
		return toolBar;
	}

	/**
	 * @return the panel's pop-up menu
	 */
	public JPopupMenu getPopupMenu() {
		return popupMenu;
	}

	/**
	 * @return the object that provides user format settings
	 */
	public FormatDTO getUserFormat() {
		return userFormat;
	}

	/**
	 * @return the decimal format
	 */
	public DecimalFormat getDecimalFormat() {
		return decimalFormat;
	}

	/**
	 * @return the formatter for columns that contain date values
	 */
	public DateTimeFormatter getDateFormat() {
		return dateFormat;
	}

	/**
	 * @return the formatter for columns that contain date time values
	 */
	public DateTimeFormatter getDateTimeFormat() {
		return dateTimeFormat;
	}

	/**
	 * @return the sort key access object
	 */
	protected SortKeyAccessor<T> createSortKeyAccessor() {
		return this::getComparableProperty;
	}

	/**
	 * Mouse listener class
	 */
	private static final class MouseListener extends MouseAdapter {
		private final JPopupMenu popup;

		/**
		 * Constructor
		 * @param popup
		 */
		public MouseListener(JPopupMenu popup) {
			this.popup = popup;
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
		 */
		@Override
		public void mousePressed(MouseEvent e) {
			if (e.isPopupTrigger())
				showMenu(e);
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
		 */
		@Override
		public void mouseReleased(MouseEvent e) {
			if (e.isPopupTrigger())
				showMenu(e);
		}

		/**
		 * @param e
		 */
		private void showMenu(MouseEvent e) {
			popup.show(e.getComponent(), e.getX(), e.getY());
		}
	}

}
