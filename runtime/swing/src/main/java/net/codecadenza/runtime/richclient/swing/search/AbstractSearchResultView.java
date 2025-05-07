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
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.KeyStroke;
import javax.swing.event.InternalFrameAdapter;
import javax.swing.event.InternalFrameEvent;
import net.codecadenza.runtime.richclient.swing.search.panel.AbstractSearchResultDataPanel;
import net.codecadenza.runtime.richclient.swing.widget.ColumnInfo;
import net.codecadenza.runtime.search.dto.SearchDTO;
import org.slf4j.Logger;

/**
 * <p>
 * Internal frame to display data within a table control that is driven by a {@link SearchDTO}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the search result view
 */
public abstract class AbstractSearchResultView<T> extends AbstractResultView<T> implements Countable {
	private static final long serialVersionUID = 9086210224174980974L;

	private final Integer savedSearchId;

	/**
	 * Constructor
	 * @param title
	 * @param savedSearchId
	 */
	protected AbstractSearchResultView(String title, Integer savedSearchId) {
		super(title);

		this.savedSearchId = savedSearchId;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.AbstractResultView#initialize()
	 */
	@Override
	protected void initialize() {
		getLogger().debug("Initialize view");

		// Add an action listener to close the view if the user presses ESC
		final ActionListener actionListener = actionEvent -> {
			AbstractSearchResultView.this.getDesktopPane().getDesktopManager().closeFrame(AbstractSearchResultView.this);
			dispose();
		};

		final KeyStroke stroke = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);
		rootPane.registerKeyboardAction(actionListener, stroke, JComponent.WHEN_IN_FOCUSED_WINDOW);

		dataPanel = new AbstractSearchResultDataPanel<>(savedSearchId) {
			private static final long serialVersionUID = -3997738786620700023L;

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.search.Countable#countData(net.codecadenza.runtime.search.dto.SearchDTO)
			 */
			@Override
			public long countData(SearchDTO searchDTO) {
				return AbstractSearchResultView.this.countData(searchDTO);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.search.panel.AbstractResultDataPanel#initSearch()
			 */
			@Override
			public SearchDTO initSearch() {
				return AbstractSearchResultView.this.initSearch();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#fetchData()
			 */
			@Override
			public Collection<T> fetchData() {
				this.refreshFormatSettings();

				return AbstractSearchResultView.this.fetchData();
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
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellBackground(java.lang.Object, int)
			 */
			@Override
			public Color getCellBackground(T element, int columnIndex) {
				return AbstractSearchResultView.this.getCellBackground(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellFont(java.lang.Object, int)
			 */
			@Override
			public Font getCellFont(T element, int columnIndex) {
				return AbstractSearchResultView.this.getCellFont(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellForeground(java.lang.Object, int)
			 */
			@Override
			public Color getCellForeground(T element, int columnIndex) {
				return AbstractSearchResultView.this.getCellForeground(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellImage(java.lang.Object, int)
			 */
			@Override
			public Icon getCellImage(T element, int columnIndex) {
				return AbstractSearchResultView.this.getCellImage(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(T element, int columnIndex) {
				return AbstractSearchResultView.this.getCellText(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellValue(java.lang.Object, int)
			 */
			@Override
			public java.lang.Comparable<?> getCellValue(T element, int columnIndex) {
				return AbstractSearchResultView.this.getCellValue(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellExportText(java.lang.Object, int)
			 */
			@Override
			public String getCellExportText(T element, int columnIndex) {
				return AbstractSearchResultView.this.getCellExportText(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#onDblClick(java.lang.Object)
			 */
			@Override
			@SuppressWarnings("unchecked")
			public void onDblClick(Object selectedElement) {
				AbstractSearchResultView.this.onDoubleClick((T) selectedElement);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.search.panel.AbstractSearchResultDataPanel#getViewID()
			 */
			@Override
			public String getViewID() {
				return AbstractSearchResultView.this.getViewID();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.JDataTable.DeleteKeyPressedListener#onDeleteKeyPressed(java.
			 * lang.Object)
			 */
			@Override
			@SuppressWarnings("unchecked")
			public void onDeleteKeyPressed(Object selectedElement) {
				AbstractSearchResultView.this.onDeletePressed((T) selectedElement);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.JDataTable.EnterKeyPressedListener#onEnterKeyPressed(java.
			 * lang.Object)
			 */
			@Override
			@SuppressWarnings("unchecked")
			public void onEnterKeyPressed(Object selectedElement) {
				AbstractSearchResultView.this.onEnterPressed((T) selectedElement);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.JDataTable.RefreshKeyPressedListener#onRefreshKeyPressed()
			 */
			@Override
			public void onRefreshKeyPressed() {
				performFetch();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getLogger()
			 */
			@Override
			protected Logger getLogger() {
				return AbstractSearchResultView.this.getLogger();
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
				if (savedSearchId == null)
					dataPanel.saveLastQuery();
			}
		});

		getLogger().debug("View initialization finished");
	}

}
