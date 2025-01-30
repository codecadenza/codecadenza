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
package net.codecadenza.eclipse.diagram.domain.providers;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.diagram.domain.part.Messages;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.gmf.runtime.diagram.ui.editparts.IGraphicalEditPart;
import org.eclipse.gmf.runtime.emf.type.core.ElementTypeRegistry;
import org.eclipse.gmf.runtime.emf.type.core.IElementType;
import org.eclipse.gmf.runtime.emf.ui.services.modelingassistant.ModelingAssistantProvider;
import org.eclipse.gmf.runtime.notation.Diagram;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;

/**
 * <p>
 * Modeling assistant provider
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaModelingAssistantProvider extends ModelingAssistantProvider {
	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.ui.services.modelingassistant.ModelingAssistantProvider#getTypesForPopupBar(org.eclipse.
	 * core.runtime.IAdaptable)
	 */
	@Override
	public List<IElementType> getTypesForPopupBar(IAdaptable host) {
		return Collections.emptyList();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.ui.services.modelingassistant.ModelingAssistantProvider#getRelTypesOnSource(org.eclipse.
	 * core.runtime.IAdaptable)
	 */
	@Override
	public List<IElementType> getRelTypesOnSource(IAdaptable source) {
		return Collections.emptyList();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.ui.services.modelingassistant.ModelingAssistantProvider#getRelTypesOnTarget(org.eclipse.
	 * core.runtime.IAdaptable)
	 */
	@Override
	public List<IElementType> getRelTypesOnTarget(IAdaptable target) {
		return Collections.emptyList();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.ui.services.modelingassistant.ModelingAssistantProvider#getRelTypesOnSourceAndTarget(org.
	 * eclipse.core.runtime.IAdaptable, org.eclipse.core.runtime.IAdaptable)
	 */
	@Override
	public List<IElementType> getRelTypesOnSourceAndTarget(IAdaptable source, IAdaptable target) {
		return Collections.emptyList();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.ui.services.modelingassistant.ModelingAssistantProvider#getTypesForSource(org.eclipse.core.
	 * runtime.IAdaptable, org.eclipse.gmf.runtime.emf.type.core.IElementType)
	 */
	@Override
	public List<IElementType> getTypesForSource(IAdaptable target, IElementType relationshipType) {
		return Collections.emptyList();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.ui.services.modelingassistant.ModelingAssistantProvider#getTypesForTarget(org.eclipse.core.
	 * runtime.IAdaptable, org.eclipse.gmf.runtime.emf.type.core.IElementType)
	 */
	@Override
	public List<IElementType> getTypesForTarget(IAdaptable source, IElementType relationshipType) {
		return Collections.emptyList();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.ui.services.modelingassistant.ModelingAssistantProvider#selectExistingElementForSource(org.
	 * eclipse.core.runtime.IAdaptable, org.eclipse.gmf.runtime.emf.type.core.IElementType)
	 */
	@Override
	public EObject selectExistingElementForSource(IAdaptable target, IElementType relationshipType) {
		return selectExistingElement(target, getTypesForSource(target, relationshipType));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.ui.services.modelingassistant.ModelingAssistantProvider#selectExistingElementForTarget(org.
	 * eclipse.core.runtime.IAdaptable, org.eclipse.gmf.runtime.emf.type.core.IElementType)
	 */
	@Override
	public EObject selectExistingElementForTarget(IAdaptable source, IElementType relationshipType) {
		return selectExistingElement(source, getTypesForTarget(source, relationshipType));
	}

	/**
	 * @param host
	 * @param types
	 * @return the selected element
	 */
	protected EObject selectExistingElement(IAdaptable host, Collection<IElementType> types) {
		if (types.isEmpty())
			return null;

		final IGraphicalEditPart editPart = host.getAdapter(IGraphicalEditPart.class);

		if (editPart == null)
			return null;

		final var diagram = (Diagram) editPart.getRoot().getContents().getModel();
		final var elements = new HashSet<EObject>();

		for (final Iterator<EObject> it = diagram.getElement().eAllContents(); it.hasNext();) {
			final EObject element = it.next();

			if (isApplicableElement(element, types))
				elements.add(element);
		}

		if (elements.isEmpty())
			return null;

		return selectElement(elements.toArray(new EObject[elements.size()]));
	}

	/**
	 * @param element
	 * @param types
	 * @return true if the element is applicable
	 */
	protected boolean isApplicableElement(EObject element, Collection<IElementType> types) {
		final IElementType type = ElementTypeRegistry.getInstance().getElementType(element);
		return types.contains(type);
	}

	/**
	 * @param elements
	 * @return the selected element
	 */
	protected EObject selectElement(EObject[] elements) {
		final Shell shell = Display.getCurrent().getActiveShell();
		final var labelProvider = new AdapterFactoryLabelProvider(
				CodeCadenzaDiagramEditorPlugin.getInstance().getItemProvidersAdapterFactory());

		final var dialog = new ElementListSelectionDialog(shell, labelProvider);
		dialog.setMessage(Messages.CodeCadenzaModelingAssistantProviderMessage);
		dialog.setTitle(Messages.CodeCadenzaModelingAssistantProviderTitle);
		dialog.setMultipleSelection(false);
		dialog.setElements(elements);

		EObject selected = null;

		if (dialog.open() == Window.OK)
			selected = (EObject) dialog.getFirstResult();

		return selected;
	}

}
