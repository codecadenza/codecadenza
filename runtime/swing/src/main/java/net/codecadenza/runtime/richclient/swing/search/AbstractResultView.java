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
package net.codecadenza.runtime.richclient.swing.search;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.text.DecimalFormat;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JInternalFrame;
import javax.swing.JPopupMenu;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.event.InternalFrameAdapter;
import javax.swing.event.InternalFrameEvent;
import net.codecadenza.runtime.richclient.swing.search.panel.AbstractResultDataPanel;
import net.codecadenza.runtime.richclient.swing.widget.ColumnInfo;
import net.codecadenza.runtime.richclient.swing.widget.StatusReceivable;
import net.codecadenza.runtime.search.dto.SearchDTO;
import org.slf4j.Logger;

/**
 * <p>
 * Internal frame to display data within a table control
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the result view
 */
public abstract class AbstractResultView<T> extends JInternalFrame implements StatusReceivable {
	private static final long serialVersionUID = 9086210224174980974L;

	protected AbstractResultDataPanel<T> dataPanel;
	protected boolean initialized;
	protected transient DateTimeFormatter dateFormat;
	protected transient DateTimeFormatter dateTimeFormat;
	protected DecimalFormat decimalFormat;

	/**
	 * Constructor
	 * @param title
	 */
	protected AbstractResultView(String title) {
		super(title);
	}

	/**
	 * @return a globally unique name of this view
	 */
	public abstract String getViewID();

	/**
	 * @return the initialized search object
	 */
	public abstract SearchDTO initSearch();

	/**
	 * Operation to get data
	 * @return a list containing data
	 */
	public abstract Collection<T> fetchData();

	/**
	 * Every non-abstract subclass must provide a logger instance!
	 * @return the logger
	 */
	protected abstract Logger getLogger();

	/**
	 * Refresh format settings
	 */
	public void refreshFormatSettings() {
		dataPanel.refreshFormatSettings();
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
	 * Callback method that is executed if a user double-clicks a data panel's row
	 * @param element
	 */
	@SuppressWarnings("unused")
	public void onDoubleClick(T element) {

	}

	/**
	 * Callback that is invoked if a user presses the enter key
	 * @param element
	 */
	@SuppressWarnings("unused")
	public void onEnterPressed(T element) {

	}

	/**
	 * Callback that is invoked if a user presses the delete key
	 * @param element
	 */
	@SuppressWarnings("unused")
	public void onDeletePressed(T element) {

	}

	/**
	 * Remove the element from the model
	 * @param element
	 */
	public void removeElement(T element) {
		dataPanel.removeElementFromModel(element);
	}

	/**
	 * @return the selected element
	 */
	public T getSelectedElement() {
		return dataPanel.getSelectedElement();
	}

	/**
	 * Initialize actions
	 */
	public void initActions() {

	}

	/**
	 * Initialize the toolbar
	 * @param toolBar
	 */
	@SuppressWarnings("unused")
	public void initToolBar(JToolBar toolBar) {

	}

	/**
	 * Initialize the pop-up menu
	 * @param menu
	 */
	@SuppressWarnings("unused")
	public void initPopUpMenu(JPopupMenu menu) {

	}

	/**
	 * Perform a data fetch operation
	 */
	public void performFetch() {
		dataPanel.performFetch();
	}

	/*
	 * (non-Javadoc)
	 * @see javax.swing.JComponent#setVisible(boolean)
	 */
	@Override
	public void setVisible(boolean show) {
		if (!show) {
			super.setVisible(false);
			return;
		}

		if (!initialized) {
			initialize();
			initialized = true;

			dataPanel.performFetch();
		}

		super.setVisible(show);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.StatusReceivable#setBusy(boolean)
	 */
	@Override
	public void setBusy(boolean busy) {
		dataPanel.setBusy(busy);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.StatusReceivable#setStatusIcon(javax.swing.ImageIcon)
	 */
	@Override
	public void setStatusIcon(ImageIcon statusIcon) {
		dataPanel.setStatusIcon(statusIcon);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.StatusReceivable#setStatusErrorMessage(java.lang.String)
	 */
	@Override
	public void setStatusErrorMessage(String message) {
		dataPanel.setStatusErrorMessage(message);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.StatusReceivable#setStatusInfoMessage(java.lang.String)
	 */
	@Override
	public void setStatusInfoMessage(String message) {
		dataPanel.setStatusInfoMessage(message);
	}

	/**
	 * Initialize the frame
	 */
	protected void initialize() {
		getLogger().debug("Initialize view");

		// Add an action listener to close the view if a user presses ESC
		final ActionListener actionListener = actionEvent -> {
			AbstractResultView.this.getDesktopPane().getDesktopManager().closeFrame(AbstractResultView.this);
			dispose();
		};

		final KeyStroke stroke = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);
		rootPane.registerKeyboardAction(actionListener, stroke, JComponent.WHEN_IN_FOCUSED_WINDOW);

		dataPanel = new AbstractResultDataPanel<>() {
			private static final long serialVersionUID = -1060563857825322510L;

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#fetchData()
			 */
			@Override
			public Collection<T> fetchData() {
				dataPanel.refreshFormatSettings();

				return AbstractResultView.this.fetchData();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getColumnDefinition()
			 */
			@Override
			public List<ColumnInfo> getColumnDefinition() {
				return Collections.emptyList();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.search.panel.AbstractResultDataPanel#getViewID()
			 */
			@Override
			public String getViewID() {
				return AbstractResultView.this.getViewID();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.search.AbstractSearchDataPanel#initSearch()
			 */
			@Override
			public SearchDTO initSearch() {
				return AbstractResultView.this.initSearch();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellBackground(java.lang.Object, int)
			 */
			@Override
			public Color getCellBackground(T element, int columnIndex) {
				return AbstractResultView.this.getCellBackground(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellFont(java.lang.Object, int)
			 */
			@Override
			public Font getCellFont(T element, int columnIndex) {
				return AbstractResultView.this.getCellFont(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellForeground(java.lang.Object, int)
			 */
			@Override
			public Color getCellForeground(T element, int columnIndex) {
				return AbstractResultView.this.getCellForeground(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellImage(java.lang.Object, int)
			 */
			@Override
			public Icon getCellImage(T element, int columnIndex) {
				return AbstractResultView.this.getCellImage(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(T element, int columnIndex) {
				return AbstractResultView.this.getCellText(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellValue(java.lang.Object, int)
			 */
			@Override
			public java.lang.Comparable<?> getCellValue(T element, int columnIndex) {
				return AbstractResultView.this.getCellValue(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellExportText(java.lang.Object, int)
			 */
			@Override
			public String getCellExportText(T element, int columnIndex) {
				return AbstractResultView.this.getCellExportText(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#onDblClick(java.lang.Object)
			 */
			@Override
			@SuppressWarnings("unchecked")
			public void onDblClick(Object selectedElement) {
				AbstractResultView.this.onDoubleClick((T) selectedElement);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.JDataTable.DeleteKeyPressedListener#onDeleteKeyPressed(java.
			 * lang.Object)
			 */
			@Override
			@SuppressWarnings("unchecked")
			public void onDeleteKeyPressed(Object selectedElement) {
				AbstractResultView.this.onDeletePressed((T) selectedElement);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.JDataTable.EnterKeyPressedListener#onEnterKeyPressed(java.
			 * lang.Object)
			 */
			@Override
			@SuppressWarnings("unchecked")
			public void onEnterKeyPressed(Object selectedElement) {
				AbstractResultView.this.onEnterPressed((T) selectedElement);
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
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getLogger()
			 */
			@Override
			protected Logger getLogger() {
				return AbstractResultView.this.getLogger();
			}
		};

		dateFormat = dataPanel.getDateFormat();
		dateTimeFormat = dataPanel.getDateTimeFormat();
		decimalFormat = dataPanel.getDecimalFormat();

		getContentPane().add(dataPanel, BorderLayout.CENTER);
		setClosable(true);
		setResizable(true);
		setMaximizable(true);
		setIconifiable(true);

		// Initialize actions
		initActions();

		// Initialize the toolbar
		initToolBar(dataPanel.getToolbar());

		// Initialize the pop-up menu
		initPopUpMenu(dataPanel.getPopupMenu());

		addInternalFrameListener(new InternalFrameAdapter() {
			/*
			 * (non-Javadoc)
			 * @see javax.swing.event.InternalFrameAdapter#internalFrameClosed(javax.swing.event.InternalFrameEvent)
			 */
			@Override
			public void internalFrameClosed(InternalFrameEvent e) {
				dataPanel.saveLastQuery();
			}
		});

		getLogger().debug("View initialization finished");
	}

}
