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

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_ELEMENT_COLLECTION_EDITOR_LBL_ADD;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_ELEMENT_COLLECTION_EDITOR_LBL_NUMBER_OF_ELEMENTS;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_ELEMENT_COLLECTION_EDITOR_MSG_CONVERSION_FAILED;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_ELEMENT_COLLECTION_EDITOR_MSG_TITLE_CONVERSION;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_ADD;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_DELETE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_DELETE_ALL;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslationForFieldLabel;

import java.util.Collection;
import java.util.Collections;
import net.codecadenza.runtime.conversion.ValueConverter;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Abstract generic base class for all element collection editors
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of an element in the collection
 */
public abstract class __AbstractElementCollectionEditor<T> extends Composite {
	protected final boolean readonly;
	protected final Class<T> elementType;
	protected final ValueConverter<T> valueConverter;
	protected final FormatDTO formatPreferences;
	protected Collection<T> elements = Collections.emptyList();
	protected ListViewer listViewer;
	protected Label lblNumberOfElements;

	/**
	 * Create the editor
	 * @param parent
	 * @param style
	 * @param readonly
	 * @param elementType
	 */
	protected __AbstractElementCollectionEditor(final Composite parent, int style, boolean readonly, Class<T> elementType) {
		super(parent, style);

		this.formatPreferences = getFormatPreferences();
		this.readonly = readonly;
		this.elementType = elementType;
		this.valueConverter = new ValueConverter<>(formatPreferences.getDecimalFormat(), formatPreferences.getDateTimeFormat(),
				formatPreferences.getDateFormat(), elementType);

		this.initComponent();
	}

	/**
	 * Set the initial elements and refresh the grid
	 * @param elements the initial elements
	 */
	public void setElements(Collection<T> elements) {
		this.elements = elements;

		refreshListView(null);
	}

	/**
	 * @return the format preferences
	 */
	public abstract FormatDTO getFormatPreferences();

	/**
	 * Initialize the component
	 */
	protected void initComponent() {
		setLayout(new GridLayout());

		if (!readonly) {
			final GridLayout glAdd = new GridLayout(3, false);
			glAdd.marginWidth = 0;
			glAdd.marginHeight = 0;
			glAdd.horizontalSpacing = 10;
			glAdd.verticalSpacing = 10;

			final var panAdd = new Composite(this, SWT.NONE);
			panAdd.setLayoutData(new GridData(SWT.FILL, SWT.NONE, true, false));
			panAdd.setLayout(glAdd);

			final var lblAdd = new Label(panAdd, SWT.NONE);
			lblAdd.setText(getTranslationForFieldLabel(ABSTRACT_ELEMENT_COLLECTION_EDITOR_LBL_ADD));

			final var txtNewElementName = new Text(panAdd, SWT.BORDER);
			txtNewElementName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtNewElementName.setText(valueConverter.getInitialDefaultValue());
			txtNewElementName.addModifyListener(event -> refreshListView(txtNewElementName.getText()));

			final var cmdAdd = new Button(panAdd, SWT.NONE);
			cmdAdd.setText(getTranslation(CMD_ADD));

			cmdAdd.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (txtNewElementName.getText().isEmpty())
						return;

					try {
						elements.add(valueConverter.convertToValue(txtNewElementName.getText()));
						refreshListView(null);
					}
					catch (final Exception ex) {
						MessageDialog.openError(getShell(), getTranslation(ABSTRACT_ELEMENT_COLLECTION_EDITOR_MSG_TITLE_CONVERSION),
								getTranslation(ABSTRACT_ELEMENT_COLLECTION_EDITOR_MSG_CONVERSION_FAILED, ex.getMessage()));
						txtNewElementName.setFocus();
						return;
					}

					listViewer.setInput(elements);
				}
			});
		}

		listViewer = new ListViewer(this, SWT.V_SCROLL | SWT.MULTI | SWT.H_SCROLL | SWT.BORDER);
		listViewer.setContentProvider(new ListContentProvider());

		lblNumberOfElements = new Label(this, SWT.NONE);

		final List list = listViewer.getList();
		list.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		if (!readonly) {
			listViewer.getList().addKeyListener(new KeyAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
				 */
				@Override
				public void keyReleased(KeyEvent e) {
					if (e.character == SWT.DEL)
						deleteSelectedElement();
				}
			});

			final var menu = new Menu(list);
			list.setMenu(menu);

			final var mniDelete = new MenuItem(menu, SWT.NONE);
			mniDelete.setText(getTranslation(CMD_DELETE));

			mniDelete.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					deleteSelectedElement();
				}
			});

			final var minDeleteAll = new MenuItem(menu, SWT.NONE);
			minDeleteAll.setText(getTranslation(CMD_DELETE_ALL));

			minDeleteAll.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					elements.clear();
					refreshListView(null);
				}
			});
		}
	}

	/**
	 * Refresh the {@link ListViewer}
	 * @param filter the filter for elements to be displayed
	 */
	protected void refreshListView(String filter) {
		final Collection<String> viewModel;

		if (filter == null || filter.isEmpty())
			viewModel = elements.stream().sorted().map(valueConverter::convertToString).toList();
		else
			viewModel = elements.stream().sorted().map(valueConverter::convertToString).filter(item -> item.startsWith(filter))
					.toList();

		lblNumberOfElements.setText(getTranslation(ABSTRACT_ELEMENT_COLLECTION_EDITOR_LBL_NUMBER_OF_ELEMENTS, elements.size()));

		listViewer.setInput(viewModel);
	}

	/**
	 * Delete the selected element from the {@link ListViewer}
	 */
	protected void deleteSelectedElement() {
		final var selection = (StructuredSelection) listViewer.getSelection();

		if (selection == null)
			return;

		final String selectedElement = (String) selection.getFirstElement();

		if (selectedElement == null)
			return;

		elements.remove(valueConverter.convertToValue(selectedElement));

		refreshListView(null);
	}

	/**
	 * Content provider
	 */
	private static class ListContentProvider implements IStructuredContentProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Object[] getElements(Object inputElement) {
			return ((Collection<String>) inputElement).toArray();
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

}
