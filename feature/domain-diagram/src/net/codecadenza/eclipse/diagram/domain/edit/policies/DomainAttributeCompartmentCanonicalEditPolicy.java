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
package net.codecadenza.eclipse.diagram.domain.edit.policies;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainAttributeEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramUpdater;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaNodeDescriptor;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalEditPolicy;
import org.eclipse.gmf.runtime.notation.View;

/**
 * <p>
 * Policy class
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainAttributeCompartmentCanonicalEditPolicy extends CanonicalEditPolicy {
	private Set<EStructuralFeature> myFeaturesToSynchronize;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalEditPolicy#getSemanticChildrenList()
	 */
	@Override
	protected List<EObject> getSemanticChildrenList() {
		final var viewObject = (View) getHost().getModel();
		final var result = new LinkedList<EObject>();

		for (final CodeCadenzaNodeDescriptor nodeDescriptor : CodeCadenzaDiagramUpdater
				.getDomainObjectAttributeCompartment_7001SemanticChildren(viewObject))
			result.add(nodeDescriptor.getModelElement());

		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalEditPolicy#isOrphaned(java.util.Collection,
	 * org.eclipse.gmf.runtime.notation.View)
	 */
	@Override
	protected boolean isOrphaned(Collection<EObject> semanticChildren, final View view) {
		final int visualID = CodeCadenzaVisualIDRegistry.getVisualID(view);

		switch (visualID) {
			case DomainAttributeEditPart.VISUAL_ID:
				if (!semanticChildren.contains(view.getElement()))
					return true;
		}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalEditPolicy#getDefaultFactoryHint()
	 */
	@Override
	protected String getDefaultFactoryHint() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalEditPolicy#getFeaturesToSynchronize()
	 */
	@Override
	protected Set<EStructuralFeature> getFeaturesToSynchronize() {
		if (myFeaturesToSynchronize == null) {
			myFeaturesToSynchronize = new HashSet<>();
			myFeaturesToSynchronize.add(DomainPackage.eINSTANCE.getDomainObject_Attributes());
		}

		return myFeaturesToSynchronize;
	}

}
