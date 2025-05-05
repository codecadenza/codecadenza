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
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
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

		listViewer.setInput(elements);
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
			txtNewElementName.selectAll();

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
		listViewer.setLabelProvider(new ListLabelProvider());
		listViewer.setContentProvider(new ListContentProvider());

		final List list = listViewer.getList();
		list.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		if (!readonly) {
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
				@SuppressWarnings("unchecked")
				public void widgetSelected(SelectionEvent e) {
					final var selection = (StructuredSelection) listViewer.getSelection();

					if (selection == null)
						return;

					final T selectedElement = (T) selection.getFirstElement();

					if (selectedElement == null)
						return;

					elements.remove(selectedElement);
					listViewer.setInput(elements);
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
					listViewer.setInput(elements);
				}
			});
		}
	}

	/**
	 * Content provider
	 */
	private class ListContentProvider implements IStructuredContentProvider {
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
	private class ListLabelProvider extends LabelProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public String getText(Object element) {
			final T item = (T) element;
			return valueConverter.convertToString(item);
		}
	}

}
