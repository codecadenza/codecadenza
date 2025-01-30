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
package net.codecadenza.eclipse.diagram.domain.sheet.custom;

import net.codecadenza.eclipse.diagram.domain.edit.parts.OneToManyAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.service.domain.DomainObjectService;
import org.eclipse.emf.transaction.RecordingCommand;
import org.eclipse.gmf.runtime.notation.Edge;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.properties.tabbed.AbstractPropertySection;
import org.eclipse.ui.views.properties.tabbed.ITabbedPropertyConstants;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;

/**
 * <p>
 * Property section for one-to-many associations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class O2MAssociationPropertySection extends AbstractPropertySection {
	private static final int LABEL_WIDTH = 220;

	private Text txtName;
	private Button chkFetchTypeEager;
	private Button chkCascadePersist;
	private Button chkCascadeRefresh;
	private Button chkCascadeMerge;
	private Button chkCascadeRemove;
	private OneToManyAssociation association;
	private OneToManyAssociationEditPart part;

	/**
	 * Listener for responding to selection changes
	 */
	private class SelectionChangeListener extends SelectionAdapter {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
		 */
		@Override
		public void widgetSelected(SelectionEvent e) {
			// Save the changes
			part.getEditingDomain().getCommandStack().execute(new RecordingCommand(part.getEditingDomain()) {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.emf.transaction.RecordingCommand#doExecute()
				 */
				@Override
				protected void doExecute() {
					association.setFetchTypeEager(chkFetchTypeEager.getSelection());
					association.setCascadePersist(chkCascadePersist.getSelection());
					association.setCascadeRefresh(chkCascadeRefresh.getSelection());
					association.setCascadeMerge(chkCascadeMerge.getSelection());
					association.setCascadeRemove(chkCascadeRemove.getSelection());
				}
			});

			// Rebuild the respective domain object source files
			try {
				final var domainObjectService = new DomainObjectService(association.getDomainObject().getNamespace().getProject());
				domainObjectService.rebuildDomainObjectSourceFiles(association.getDomainObject(), true);
				domainObjectService.rebuildDomainObjectSourceFiles(association.getTarget(), true);
			}
			catch (final Exception ex) {
				CodeCadenzaDiagramEditorPlugin.getInstance().handleInternalError(ex);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#createControls(org.eclipse.swt.widgets.Composite,
	 * org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage)
	 */
	@Override
	public void createControls(Composite parent, TabbedPropertySheetPage aTabbedPropertySheetPage) {
		super.createControls(parent, aTabbedPropertySheetPage);

		final Composite panPropertySection = getWidgetFactory().createFlatFormComposite(parent);

		var data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(0, ITabbedPropertyConstants.VSPACE);

		txtName = getWidgetFactory().createText(panPropertySection, "", SWT.READ_ONLY);
		txtName.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtName, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtName, 0, SWT.CENTER);

		final CLabel lblName = getWidgetFactory().createCLabel(panPropertySection, "Name:");
		lblName.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtName, ITabbedPropertyConstants.VSPACE);

		chkFetchTypeEager = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkFetchTypeEager.setLayoutData(data);
		chkFetchTypeEager.addSelectionListener(new SelectionChangeListener());

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkFetchTypeEager, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkFetchTypeEager, 0, SWT.CENTER);

		final CLabel lblFetchType = getWidgetFactory().createCLabel(panPropertySection, "Eager fetch:");
		lblFetchType.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(chkFetchTypeEager, ITabbedPropertyConstants.VSPACE);

		chkCascadePersist = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkCascadePersist.setLayoutData(data);
		chkCascadePersist.addSelectionListener(new SelectionChangeListener());

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkCascadePersist, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkCascadePersist, 0, SWT.CENTER);

		final CLabel lblPersist = getWidgetFactory().createCLabel(panPropertySection, "Cascade persist:");
		lblPersist.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(chkCascadePersist, ITabbedPropertyConstants.VSPACE);

		chkCascadeRefresh = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkCascadeRefresh.setLayoutData(data);
		chkCascadeRefresh.addSelectionListener(new SelectionChangeListener());

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkCascadeRefresh, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkCascadeRefresh, 0, SWT.CENTER);

		final CLabel lblRefresh = getWidgetFactory().createCLabel(panPropertySection, "Cascade refresh:");
		lblRefresh.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(chkCascadeRefresh, ITabbedPropertyConstants.VSPACE);

		chkCascadeMerge = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkCascadeMerge.setLayoutData(data);
		chkCascadeMerge.addSelectionListener(new SelectionChangeListener());

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkCascadeMerge, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkCascadeMerge, 0, SWT.CENTER);

		final CLabel lblMerge = getWidgetFactory().createCLabel(panPropertySection, "Cascade merge:");
		lblMerge.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(chkCascadeMerge, ITabbedPropertyConstants.VSPACE);

		chkCascadeRemove = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkCascadeRemove.setLayoutData(data);
		chkCascadeRemove.addSelectionListener(new SelectionChangeListener());

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkCascadeRemove, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkCascadeRemove, 0, SWT.CENTER);

		final CLabel lblRemove = getWidgetFactory().createCLabel(panPropertySection, "Cascade remove:");
		lblRemove.setLayoutData(data);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#refresh()
	 */
	@Override
	public void refresh() {
		txtName.setText(association.getName());
		chkFetchTypeEager.setSelection(association.isFetchTypeEager());
		chkCascadePersist.setSelection(association.isCascadePersist());
		chkCascadeRefresh.setSelection(association.isCascadeRefresh());
		chkCascadeMerge.setSelection(association.isCascadeMerge());
		chkCascadeRemove.setSelection(association.isCascadeRemove());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#setInput(org.eclipse.ui.IWorkbenchPart,
	 * org.eclipse.jface.viewers.ISelection)
	 */
	@Override
	public void setInput(IWorkbenchPart part, ISelection selection) {
		super.setInput(part, selection);

		final Object input = ((IStructuredSelection) selection).getFirstElement();
		this.part = (OneToManyAssociationEditPart) input;
		this.association = (OneToManyAssociation) ((Edge) ((OneToManyAssociationEditPart) input).getModel()).getElement();
	}

}
