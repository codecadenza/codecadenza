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
package net.codecadenza.eclipse.ui.dialog.testing.integration.util;

import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;

/**
 * <p>
 * Class that adds a drop listener to the given {@link DropTarget} so that a respective control can react on received tracking
 * attributes
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractTrackingAttributeDropListener {
	protected final DropTarget dropTarget;
	protected final IntegrationMethodTestInvocation methodInvocation;
	protected final IntegrationTestCase testCase;
	protected final Project project;

	/**
	 * Constructor
	 * @param dropTarget
	 * @param testModule
	 * @param testCase
	 * @param methodInvocation
	 */
	protected AbstractTrackingAttributeDropListener(DropTarget dropTarget, IntegrationTestModule testModule,
			IntegrationTestCase testCase, IntegrationMethodTestInvocation methodInvocation) {
		this.dropTarget = dropTarget;
		this.testCase = testCase;
		this.methodInvocation = methodInvocation;
		this.project = testModule.getProject();

		init();
	}

	/**
	 * A subclass must define what should be done if a tracking attribute has been dropped on the respective control
	 * @param domainObject the domain object
	 * @param trackedAttribute the tracked attribute
	 */
	protected abstract void onTrackingAttributeReceived(DomainObject domainObject, TestDataAttribute trackedAttribute);

	/**
	 * Initialize the drop listener and add it to the {@link DropTarget}
	 */
	private void init() {
		final TextTransfer textTransfer = TextTransfer.getInstance();
		final var types = new Transfer[] { textTransfer };

		dropTarget.setTransfer(types);

		dropTarget.addDropListener(new DropTargetAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#dragEnter(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dragEnter(DropTargetEvent event) {
				event.detail = DND.DROP_COPY;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#dropAccept(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dropAccept(DropTargetEvent event) {
				event.detail = DND.DROP_COPY;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#drop(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void drop(DropTargetEvent event) {
				final String[] data = event.data.toString().split(" ");

				if (data.length != 5)
					return;

				final String domainObjectName = data[0];
				final String attributeName = data[1];
				final String id = data[2];
				final int parentIndexOfTrackingInvocation = Integer.parseInt(data[3]);
				final int nestedIndexOfTrackingInvocation = Integer.parseInt(data[4]);
				final DomainObject domainObject = project.getAllDomainObjectsOfProject(false, true).stream()
						.filter(d -> d.getName().equals(domainObjectName)).findFirst().orElse(null);

				if (domainObject == null)
					return;

				if (attributeName != null && !attributeName.isEmpty()) {
					final DomainAttribute domainAttribute = domainObject.getAllAttributes().stream()
							.filter(a -> a.getName().equals(attributeName)).findFirst().orElse(null);

					if (domainAttribute == null)
						return;
				}

				final int indexOfTargetInvocation;

				if (methodInvocation.getParentInvocation() == null)
					indexOfTargetInvocation = testCase.getMethodInvocations().indexOf(methodInvocation);
				else
					indexOfTargetInvocation = testCase.getMethodInvocations().indexOf(methodInvocation.getParentInvocation());

				// A reference on a tracking attribute that is created in a subsequent method invocation makes no sense!
				if (indexOfTargetInvocation != -1 && indexOfTargetInvocation < parentIndexOfTrackingInvocation)
					return;

				IntegrationMethodTestInvocation trackingInvocation = testCase.getMethodInvocations().get(parentIndexOfTrackingInvocation);

				// Make sure to use the selected nested invocation so that the correct tracking attribute will be referenced! There should
				// be no verification that the tracked invocation and the referenced invocation are in the same order!
				if (nestedIndexOfTrackingInvocation != -1)
					trackingInvocation = trackingInvocation.getNestedInvocations().get(nestedIndexOfTrackingInvocation);

				final TestDataAttribute trackedAttribute = trackingInvocation.getTrackedAttribute();

				if (trackedAttribute != null && trackedAttribute.getId().equals(id))
					onTrackingAttributeReceived(domainObject, trackedAttribute);
			}
		});
	}

}
