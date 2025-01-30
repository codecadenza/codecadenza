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

import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import org.eclipse.emf.transaction.RecordingCommand;
import org.eclipse.gmf.runtime.diagram.ui.editparts.ConnectionNodeEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editparts.IGraphicalEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editparts.ShapeNodeEditPart;
import org.eclipse.gmf.runtime.notation.Edge;
import org.eclipse.gmf.runtime.notation.Node;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.properties.tabbed.AbstractPropertySection;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;

/**
 * <p>
 * Property section for entering comments regarding domain attributes and associations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CommentsPropertySection extends AbstractPropertySection {
	private Text txtComment;
	private Text txtInternalComment;
	private DomainAttribute attribute;
	private AbstractDomainAssociation association;
	private IGraphicalEditPart part;
	private ModifyListener textChangeListener;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#createControls(org.eclipse.swt.widgets.Composite,
	 * org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage)
	 */
	@Override
	public void createControls(Composite parent, TabbedPropertySheetPage aTabbedPropertySheetPage) {
		super.createControls(parent, aTabbedPropertySheetPage);

		final Composite panPropertySection = getWidgetFactory().createFlatFormComposite(parent);
		panPropertySection.setLayout(new GridLayout(2, false));

		final CLabel lblUserComment = getWidgetFactory().createCLabel(panPropertySection, "User comment:");
		lblUserComment.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));

		final var gdUserComment = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdUserComment.heightHint = 100;

		txtComment = getWidgetFactory().createText(panPropertySection, "", SWT.MULTI);
		txtComment.setLayoutData(gdUserComment);

		final CLabel lblInternalComment = getWidgetFactory().createCLabel(panPropertySection, "Internal comment:");
		lblInternalComment.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));

		final var gdInternalComment = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdInternalComment.heightHint = 100;

		txtInternalComment = getWidgetFactory().createText(panPropertySection, "", SWT.MULTI);
		txtInternalComment.setLayoutData(gdInternalComment);

		textChangeListener = e -> updateMetaModel();
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
		this.part = (IGraphicalEditPart) input;

		if (input instanceof final ShapeNodeEditPart shapeNodeEditPart)
			attribute = (DomainAttribute) ((Node) shapeNodeEditPart.getModel()).getElement();
		else
			association = (AbstractDomainAssociation) ((Edge) ((ConnectionNodeEditPart) input).getModel()).getElement();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#refresh()
	 */
	@Override
	public void refresh() {
		txtComment.removeModifyListener(textChangeListener);
		txtInternalComment.removeModifyListener(textChangeListener);

		if (attribute != null) {
			txtComment.setText(attribute.getUserComment() == null ? "" : attribute.getUserComment());
			txtInternalComment.setText(attribute.getInternalComment() == null ? "" : attribute.getInternalComment());
		}
		else {
			txtComment.setText(association.getUserComment() == null ? "" : association.getUserComment());
			txtInternalComment.setText(association.getInternalComment() == null ? "" : association.getInternalComment());
		}

		txtComment.addModifyListener(textChangeListener);
		txtInternalComment.addModifyListener(textChangeListener);
	}

	/**
	 * Update the meta-model
	 */
	private void updateMetaModel() {
		part.getEditingDomain().getCommandStack().execute(new RecordingCommand(part.getEditingDomain()) {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.transaction.RecordingCommand#doExecute()
			 */
			@Override
			protected void doExecute() {
				if (attribute != null) {
					attribute.setUserComment(txtComment.getText());
					attribute.setInternalComment(txtInternalComment.getText());
				}
				else {
					association.setUserComment(txtComment.getText());
					association.setInternalComment(txtInternalComment.getText());
				}
			}
		});
	}

}
