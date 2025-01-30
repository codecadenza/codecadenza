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
package net.codecadenza.eclipse.diagram.domain.sheet;

import java.util.ArrayList;
import java.util.Iterator;
import org.eclipse.core.commands.operations.IOperationHistory;
import org.eclipse.core.commands.operations.OperationHistoryFactory;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.emf.edit.provider.IItemPropertySource;
import org.eclipse.emf.edit.ui.provider.PropertySource;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.emf.transaction.util.TransactionUtil;
import org.eclipse.gef.EditPart;
import org.eclipse.gmf.runtime.diagram.ui.properties.sections.AdvancedPropertySection;
import org.eclipse.gmf.runtime.emf.ui.properties.sections.PropertySheetEntry;
import org.eclipse.gmf.runtime.emf.ui.properties.sections.UndoableModelPropertySheetEntry;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.properties.IPropertySource;
import org.eclipse.ui.views.properties.IPropertySourceProvider;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;

/**
 * <p>
 * Property section
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaPropertySection extends AdvancedPropertySection implements IPropertySourceProvider {
	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.IPropertySourceProvider#getPropertySource(java.lang.Object)
	 */
	@Override
	public IPropertySource getPropertySource(Object object) {
		if (object instanceof final IPropertySource propertySource)
			return propertySource;

		final AdapterFactory af = getAdapterFactory(object);

		if (af != null) {
			final var ips = (IItemPropertySource) af.adapt(object, IItemPropertySource.class);

			if (ips != null)
				return new PropertySource(object, ips);
		}

		if (object instanceof final IAdaptable adaptable)
			return adaptable.getAdapter(IPropertySource.class);

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.properties.sections.AdvancedPropertySection#createControls(org.eclipse.swt.widgets.
	 * Composite, org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage)
	 */
	@Override
	public void createControls(Composite parent, TabbedPropertySheetPage aTabbedPropertySheetPage) {
		super.createControls(parent, aTabbedPropertySheetPage);

		class ROEntry extends UndoableModelPropertySheetEntry {
			/**
			 * @param operationHistory
			 */
			ROEntry(IOperationHistory operationHistory) {
				super(operationHistory);
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.gmf.runtime.emf.ui.properties.sections.PropertySheetEntry#getEditor(org.eclipse.swt.widgets.Composite)
			 */
			@Override
			public CellEditor getEditor(Composite parentComposite) {
				return null; // do not allow editing
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.gmf.runtime.emf.ui.properties.sections.UndoableModelPropertySheetEntry#createChildEntry()
			 */
			@Override
			protected PropertySheetEntry createChildEntry() {
				return new ROEntry(getOperationHistory());
			}
		}

		final var root = new ROEntry(OperationHistoryFactory.getOperationHistory());
		root.setPropertySourceProvider(getPropertySourceProvider());

		page.setRootEntry(root);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.properties.sections.AdvancedPropertySection#getPropertySourceProvider()
	 */
	@Override
	protected IPropertySourceProvider getPropertySourceProvider() {
		return this;
	}

	/**
	 * Transform the selection
	 * @param selected
	 * @return the transformed selection
	 */
	protected Object transformSelection(Object selected) {
		if (selected instanceof final EditPart editPart) {
			final Object model = editPart.getModel();
			return model instanceof final View view ? view.getElement() : null;
		}

		if (selected instanceof final View view)
			return view.getElement();

		if (selected instanceof final IAdaptable adaptable) {
			final View view = adaptable.getAdapter(View.class);

			if (view != null)
				return view.getElement();
		}

		return selected;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.properties.sections.AdvancedPropertySection#setInput(org.eclipse.ui.IWorkbenchPart,
	 * org.eclipse.jface.viewers.ISelection)
	 */
	@Override
	public void setInput(IWorkbenchPart part, ISelection selection) {
		if (selection.isEmpty() || !(selection instanceof final StructuredSelection structuredSelection)) {
			super.setInput(part, selection);
			return;
		}

		final var transformedSelection = new ArrayList<>(structuredSelection.size());

		for (final Iterator<?> it = structuredSelection.iterator(); it.hasNext();) {
			final Object r = transformSelection(it.next());

			if (r != null)
				transformedSelection.add(r);
		}

		super.setInput(part, new StructuredSelection(transformedSelection));
	}

	/**
	 * @param object
	 * @return the adapter factory
	 */
	protected AdapterFactory getAdapterFactory(Object object) {
		if (getEditingDomain() instanceof final AdapterFactoryEditingDomain adapterFactoryEditingDomain)
			return adapterFactoryEditingDomain.getAdapterFactory();

		final TransactionalEditingDomain editingDomain = TransactionUtil.getEditingDomain(object);

		if (editingDomain != null)
			return ((AdapterFactoryEditingDomain) editingDomain).getAdapterFactory();

		return null;
	}

}
