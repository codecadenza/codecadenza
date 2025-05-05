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
package net.codecadenza.eclipse.diagram.domain.edit.parts;

import static net.codecadenza.eclipse.shared.Constants.IMG_ENUM;

import java.util.Collections;
import java.util.List;
import net.codecadenza.eclipse.diagram.domain.edit.policies.CodeCadenzaTextSelectionEditPolicy;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.diagram.domain.providers.CodeCadenzaElementTypes;
import net.codecadenza.eclipse.diagram.domain.providers.CodeCadenzaParserProvider;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.geometry.Point;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.transaction.RunnableWithResult;
import org.eclipse.gef.AccessibleEditPart;
import org.eclipse.gef.EditPolicy;
import org.eclipse.gef.Request;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.editpolicies.NonResizableEditPolicy;
import org.eclipse.gef.requests.DirectEditRequest;
import org.eclipse.gef.tools.DirectEditManager;
import org.eclipse.gmf.runtime.common.ui.services.parser.IParser;
import org.eclipse.gmf.runtime.common.ui.services.parser.IParserEditStatus;
import org.eclipse.gmf.runtime.common.ui.services.parser.ParserEditStatus;
import org.eclipse.gmf.runtime.common.ui.services.parser.ParserOptions;
import org.eclipse.gmf.runtime.common.ui.services.parser.ParserService;
import org.eclipse.gmf.runtime.diagram.ui.editparts.CompartmentEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editparts.IGraphicalEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editparts.ITextAwareEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editpolicies.LabelDirectEditPolicy;
import org.eclipse.gmf.runtime.diagram.ui.l10n.DiagramColorRegistry;
import org.eclipse.gmf.runtime.diagram.ui.requests.RequestConstants;
import org.eclipse.gmf.runtime.diagram.ui.tools.TextDirectEditManager;
import org.eclipse.gmf.runtime.draw2d.ui.figures.WrappingLabel;
import org.eclipse.gmf.runtime.emf.core.util.EObjectAdapter;
import org.eclipse.gmf.runtime.emf.ui.services.parser.ISemanticParser;
import org.eclipse.gmf.runtime.notation.FontStyle;
import org.eclipse.gmf.runtime.notation.NotationPackage;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.viewers.ICellEditorValidator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.accessibility.AccessibleEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Image;

/**
 * <p>
 * Enumeration name edit part
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaEnumNameEditPart extends CompartmentEditPart implements ITextAwareEditPart {
	public static final int VISUAL_ID = 5004;
	private DirectEditManager manager;
	private IParser parser;
	private List<?> parserElements;
	private String defaultText;

	/**
	 * @param view
	 */
	public JavaEnumNameEditPart(View view) {
		super(view);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.CompartmentEditPart#createDefaultEditPolicies()
	 */
	@Override
	protected void createDefaultEditPolicies() {
		super.createDefaultEditPolicies();

		installEditPolicy(EditPolicy.DIRECT_EDIT_ROLE, new LabelDirectEditPolicy());

		installEditPolicy(EditPolicy.PRIMARY_DRAG_ROLE, new NonResizableEditPolicy() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.gef.editpolicies.NonResizableEditPolicy#getCommand(org.eclipse.gef.Request)
			 */
			@Override
			public Command getCommand(Request request) {
				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.gef.editpolicies.NonResizableEditPolicy#understandsRequest(org.eclipse.gef.Request)
			 */
			@Override
			public boolean understandsRequest(Request request) {
				return false;
			}
		});
	}

	/**
	 * @param figure
	 * @return the label text helper string
	 */
	protected String getLabelTextHelper(IFigure figure) {
		if (figure instanceof final WrappingLabel wrappingLabel)
			return wrappingLabel.getText();

		return ((Label) figure).getText();
	}

	/**
	 * @param figure
	 * @param text
	 */
	protected void setLabelTextHelper(IFigure figure, String text) {
		if (figure instanceof final WrappingLabel wrappingLabel)
			wrappingLabel.setText(text);
		else
			((Label) figure).setText(text);
	}

	/**
	 * @param figure
	 * @return the image
	 */
	protected Image getLabelIconHelper(IFigure figure) {
		if (figure instanceof final WrappingLabel wrappingLabel)
			return wrappingLabel.getIcon();

		return ((Label) figure).getIcon();
	}

	/**
	 * @param figure
	 * @param icon
	 */
	protected void setLabelIconHelper(IFigure figure, Image icon) {
		if (figure instanceof final WrappingLabel wrappingLabel)
			wrappingLabel.setIcon(icon);
		else
			((Label) figure).setIcon(icon);
	}

	/**
	 * @param figure
	 */
	public void setLabel(WrappingLabel figure) {
		unregisterVisuals();
		setFigure(figure);
		defaultText = getLabelTextHelper(figure);
		registerVisuals();
		refreshVisuals();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.GraphicalEditPart#getModelChildren()
	 */
	@Override
	protected List<?> getModelChildren() {
		return Collections.emptyList();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.GraphicalEditPart#getChildBySemanticHint(java.lang.String)
	 */
	@Override
	public IGraphicalEditPart getChildBySemanticHint(String semanticHint) {
		return null;
	}

	/**
	 * @return the parser element
	 */
	protected EObject getParserElement() {
		return resolveSemanticElement();
	}

	/**
	 * @return the label icon
	 */
	protected Image getLabelIcon() {
		final EObject parserElement = getParserElement();

		if (parserElement == null)
			return null;

		Image retImage = null;

		if (parserElement instanceof JavaEnum)
			retImage = CodeCadenzaResourcePlugin.getImage(IMG_ENUM);

		return retImage;
	}

	/**
	 * @return the label text
	 */
	protected String getLabelText() {
		String text = null;
		final EObject parserElement = getParserElement();

		if (parserElement != null && getParser() != null)
			text = getParser().getPrintString(new EObjectAdapter(parserElement), getParserOptions().intValue());

		if (text == null || text.isEmpty())
			text = defaultText;

		return text;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.ITextAwareEditPart#setLabelText(java.lang.String)
	 */
	@Override
	public void setLabelText(String text) {
		setLabelTextHelper(getFigure(), text);
		final Object pdEditPolicy = getEditPolicy(EditPolicy.PRIMARY_DRAG_ROLE);

		if (pdEditPolicy instanceof final CodeCadenzaTextSelectionEditPolicy textSelectionEditPolicy)
			textSelectionEditPolicy.refreshFeedback();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.ITextAwareEditPart#getEditText()
	 */
	@Override
	public String getEditText() {
		if (getParserElement() == null || getParser() == null)
			return "";

		return getParser().getEditString(new EObjectAdapter(getParserElement()), getParserOptions().intValue());
	}

	/**
	 * @return always false
	 */
	protected boolean isEditable() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.ITextAwareEditPart#getEditTextValidator()
	 */
	@Override
	public ICellEditorValidator getEditTextValidator() {
		return value -> {
			if (value instanceof final String stringValue) {
				final EObject element = getParserElement();
				final IParser parserInstance = getParser();

				try {
					final var valid = (IParserEditStatus) getEditingDomain().runExclusive(new RunnableWithResult.Impl<IParserEditStatus>() {
						/*
						 * (non-Javadoc)
						 * @see java.lang.Runnable#run()
						 */
						@Override
						public void run() {
							setResult(parserInstance.isValidEditString(new EObjectAdapter(element), stringValue));
						}
					});

					return valid.getCode() == ParserEditStatus.EDITABLE ? null : valid.getMessage();
				}
				catch (final InterruptedException ie) {
					Thread.currentThread().interrupt();

					CodeCadenzaDiagramEditorPlugin.getInstance().logInfo("Operation has been interrupted!");
				}
			}

			// Shouldn't get here
			return null;
		};
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.ITextAwareEditPart#getCompletionProcessor()
	 */
	@Override
	public IContentAssistProcessor getCompletionProcessor() {
		if (getParserElement() == null || getParser() == null)
			return null;

		return getParser().getCompletionProcessor(new EObjectAdapter(getParserElement()));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.ITextAwareEditPart#getParserOptions()
	 */
	@Override
	public ParserOptions getParserOptions() {
		return ParserOptions.NONE;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.ITextAwareEditPart#getParser()
	 */
	@Override
	public IParser getParser() {
		if (parser == null) {
			final String parserHint = ((View) getModel()).getType();
			final var hintAdapter = new CodeCadenzaParserProvider.HintAdapter(CodeCadenzaElementTypes.JavaEnum_2002, getParserElement(),
					parserHint);

			parser = ParserService.getInstance().getParser(hintAdapter);
		}

		return parser;
	}

	/**
	 * @return the edit manager
	 */
	@SuppressWarnings("deprecation")
	protected DirectEditManager getManager() {
		if (manager == null)
			setManager(new TextDirectEditManager(this, TextDirectEditManager.getTextCellEditorClass(this),
					CodeCadenzaEditPartFactory.getTextCellEditorLocator(this)));

		return manager;
	}

	/**
	 * @param manager
	 */
	protected void setManager(DirectEditManager manager) {
		this.manager = manager;
	}

	/**
	 * Perform the direct edit operation
	 */
	protected void performDirectEdit() {
		getManager().show();
	}

	/**
	 * @param eventLocation
	 */
	protected void performDirectEdit(Point eventLocation) {
		if (getManager().getClass() == TextDirectEditManager.class)
			((TextDirectEditManager) getManager()).show(eventLocation.getSWTPoint());
	}

	/**
	 * @param initialCharacter
	 */
	private void performDirectEdit(char initialCharacter) {
		if (getManager() instanceof final TextDirectEditManager textDirectEditManager)
			textDirectEditManager.show(initialCharacter);
		else
			performDirectEdit();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.GraphicalEditPart#performDirectEditRequest(org.eclipse.gef.Request)
	 */
	@Override
	protected void performDirectEditRequest(Request request) {
		final Request theRequest = request;

		try {
			getEditingDomain().runExclusive(() -> {
				if (isActive() && isEditable()) {
					final var ch = theRequest.getExtendedData().get(RequestConstants.REQ_DIRECTEDIT_EXTENDEDDATA_INITIAL_CHAR);

					if (ch instanceof final Character initialChar)
						performDirectEdit(initialChar);
					else if (theRequest instanceof final DirectEditRequest editRequest && getEditText().equals(getLabelText()))
						performDirectEdit(editRequest.getLocation());
					else
						performDirectEdit();
				}
			});
		}
		catch (final InterruptedException e) {
			Thread.currentThread().interrupt();

			CodeCadenzaDiagramEditorPlugin.getInstance().logInfo("Operation has been interrupted!");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.GraphicalEditPart#refreshVisuals()
	 */
	@Override
	protected void refreshVisuals() {
		super.refreshVisuals();

		refreshLabel();
		refreshFont();
		refreshFontColor();
		refreshUnderline();
		refreshStrikeThrough();
	}

	/**
	 * Refresh the label
	 */
	protected void refreshLabel() {
		setLabelTextHelper(getFigure(), getLabelText());
		setLabelIconHelper(getFigure(), getLabelIcon());
		final Object pdEditPolicy = getEditPolicy(EditPolicy.PRIMARY_DRAG_ROLE);

		if (pdEditPolicy instanceof final CodeCadenzaTextSelectionEditPolicy textSelectionEditPolicy)
			textSelectionEditPolicy.refreshFeedback();
	}

	/**
	 * Refresh the underline
	 */
	protected void refreshUnderline() {
		final var style = (FontStyle) getFontStyleOwnerView().getStyle(NotationPackage.eINSTANCE.getFontStyle());

		if (style != null && getFigure() instanceof final WrappingLabel wrappingLabel)
			wrappingLabel.setTextUnderline(style.isUnderline());
	}

	/**
	 * Refresh the strike-through
	 */
	protected void refreshStrikeThrough() {
		final var style = (FontStyle) getFontStyleOwnerView().getStyle(NotationPackage.eINSTANCE.getFontStyle());

		if (style != null && getFigure() instanceof final WrappingLabel wrappingLabel)
			wrappingLabel.setTextStrikeThrough(style.isStrikeThrough());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.GraphicalEditPart#refreshFont()
	 */
	@Override
	protected void refreshFont() {
		final var style = (FontStyle) getFontStyleOwnerView().getStyle(NotationPackage.eINSTANCE.getFontStyle());

		if (style != null) {
			final var fontData = new FontData(style.getFontName(), style.getFontHeight(),
					(style.isBold() ? SWT.BOLD : SWT.NORMAL) | (style.isItalic() ? SWT.ITALIC : SWT.NORMAL));
			setFont(fontData);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.GraphicalEditPart#setFontColor(org.eclipse.swt.graphics.Color)
	 */
	@Override
	protected void setFontColor(Color color) {
		getFigure().setForegroundColor(color);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.GraphicalEditPart#addSemanticListeners()
	 */
	@Override
	protected void addSemanticListeners() {
		if (getParser() instanceof final ISemanticParser semanticParser) {
			final EObject element = resolveSemanticElement();
			parserElements = semanticParser.getSemanticElementsBeingParsed(element);

			for (int i = 0; i < parserElements.size(); i++)
				addListenerFilter("SemanticModel" + i, this, (EObject) parserElements.get(i));
		}
		else
			super.addSemanticListeners();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.GraphicalEditPart#removeSemanticListeners()
	 */
	@Override
	protected void removeSemanticListeners() {
		if (parserElements != null) {
			for (int i = 0; i < parserElements.size(); i++)
				removeListenerFilter("SemanticModel" + i);
		}
		else
			super.removeSemanticListeners();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.GraphicalEditPart#getAccessibleEditPart()
	 */
	@Override
	protected AccessibleEditPart getAccessibleEditPart() {
		if (accessibleEP == null) {
			accessibleEP = new AccessibleGraphicalEditPart() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.gef.AccessibleEditPart#getName(org.eclipse.swt.accessibility.AccessibleEvent)
				 */
				@Override
				public void getName(AccessibleEvent e) {
					e.result = getLabelTextHelper(getFigure());
				}
			};
		}

		return accessibleEP;
	}

	/**
	 * @return the view
	 */
	private View getFontStyleOwnerView() {
		return (View) getModel();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.GraphicalEditPart#addNotationalListeners()
	 */
	@Override
	protected void addNotationalListeners() {
		super.addNotationalListeners();
		addListenerFilter("PrimaryView", this, getPrimaryView());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.GraphicalEditPart#removeNotationalListeners()
	 */
	@Override
	protected void removeNotationalListeners() {
		super.removeNotationalListeners();
		removeListenerFilter("PrimaryView");
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.GraphicalEditPart#handleNotificationEvent(org.eclipse.emf.common.notify.
	 * Notification)
	 */
	@Override
	protected void handleNotificationEvent(Notification event) {
		final Object feature = event.getFeature();

		if (NotationPackage.eINSTANCE.getFontStyle_FontColor().equals(feature)) {
			final var newValue = (Integer) event.getNewValue();
			setFontColor(DiagramColorRegistry.getInstance().getColor(newValue));
		}
		else if (NotationPackage.eINSTANCE.getFontStyle_Underline().equals(feature))
			refreshUnderline();
		else if (NotationPackage.eINSTANCE.getFontStyle_StrikeThrough().equals(feature))
			refreshStrikeThrough();
		else if (NotationPackage.eINSTANCE.getFontStyle_FontHeight().equals(feature)
				|| NotationPackage.eINSTANCE.getFontStyle_FontName().equals(feature)
				|| NotationPackage.eINSTANCE.getFontStyle_Bold().equals(feature)
				|| NotationPackage.eINSTANCE.getFontStyle_Italic().equals(feature))
			refreshFont();
		else {
			if (getParser() != null && getParser().isAffectingEvent(event, getParserOptions().intValue()))
				refreshLabel();

			if (getParser() instanceof final ISemanticParser modelParser && modelParser.areSemanticElementsAffected(null, event)) {
				removeSemanticListeners();

				if (resolveSemanticElement() != null)
					addSemanticListeners();

				refreshLabel();
			}
		}

		super.handleNotificationEvent(event);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.GraphicalEditPart#createFigure()
	 */
	@Override
	protected IFigure createFigure() {
		// The parent should assign a figure by using the setLabel() method
		return null;
	}

}
