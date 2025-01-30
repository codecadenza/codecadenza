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

import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainAttributeNameEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainObjectNameEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.EnumLiteralNameEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.JavaEnumNameEditPart;
import net.codecadenza.eclipse.diagram.domain.parsers.MessageFormatParser;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.java.JavaPackage;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.gmf.runtime.common.core.service.AbstractProvider;
import org.eclipse.gmf.runtime.common.core.service.IOperation;
import org.eclipse.gmf.runtime.common.ui.services.parser.GetParserOperation;
import org.eclipse.gmf.runtime.common.ui.services.parser.IParser;
import org.eclipse.gmf.runtime.common.ui.services.parser.IParserProvider;
import org.eclipse.gmf.runtime.emf.type.core.IElementType;
import org.eclipse.gmf.runtime.emf.ui.services.parser.ParserHintAdapter;
import org.eclipse.gmf.runtime.notation.View;

/**
 * <p>
 * Parser provider
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaParserProvider extends AbstractProvider implements IParserProvider {
	private IParser domainObjectNameParser;

	/**
	 * @return the parser
	 */
	private IParser getDomainObjectName_5002Parser() {
		if (domainObjectNameParser == null)
			domainObjectNameParser = createDomainObjectName_5002Parser();

		return domainObjectNameParser;
	}

	/**
	 * @return the parser
	 */
	protected IParser createDomainObjectName_5002Parser() {
		return new MessageFormatParser(new EAttribute[] { JavaPackage.eINSTANCE.getJavaType_Name() });
	}

	private IParser javaEnumName_5004Parser;

	/**
	 * @return the parser
	 */
	private IParser getJavaEnumName_5004Parser() {
		if (javaEnumName_5004Parser == null)
			javaEnumName_5004Parser = createJavaEnumName_5004Parser();

		return javaEnumName_5004Parser;
	}

	/**
	 * @return the parser
	 */
	protected IParser createJavaEnumName_5004Parser() {
		return new MessageFormatParser(new EAttribute[] { JavaPackage.eINSTANCE.getJavaType_Name() });
	}

	private IParser domainAttributeName_5001Parser;

	/**
	 * @return the parser
	 */
	private IParser getDomainAttributeName_5001Parser() {
		if (domainAttributeName_5001Parser == null)
			domainAttributeName_5001Parser = createDomainAttributeName_5001Parser();

		return domainAttributeName_5001Parser;
	}

	/**
	 * @return the parser
	 */
	protected IParser createDomainAttributeName_5001Parser() {
		return new MessageFormatParser(new EAttribute[] { DomainPackage.eINSTANCE.getDomainAttribute_Name() });
	}

	private IParser enumLiteralName_5003Parser;

	/**
	 * @return the parser
	 */
	private IParser getEnumLiteralName_5003Parser() {
		if (enumLiteralName_5003Parser == null)
			enumLiteralName_5003Parser = createEnumLiteralName_5003Parser();

		return enumLiteralName_5003Parser;
	}

	/**
	 * @return the parser
	 */
	protected IParser createEnumLiteralName_5003Parser() {
		return new MessageFormatParser(new EAttribute[] { JavaPackage.eINSTANCE.getEnumLiteral_Name() });
	}

	/**
	 * @param visualID
	 * @return the parser
	 */
	protected IParser getParser(int visualID) {
		switch (visualID) {
			case DomainObjectNameEditPart.VISUAL_ID:
				return getDomainObjectName_5002Parser();
			case JavaEnumNameEditPart.VISUAL_ID:
				return getJavaEnumName_5004Parser();
			case DomainAttributeNameEditPart.VISUAL_ID:
				return getDomainAttributeName_5001Parser();
			case EnumLiteralNameEditPart.VISUAL_ID:
				return getEnumLiteralName_5003Parser();
		}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.common.ui.services.parser.IParserProvider#getParser(org.eclipse.core.runtime.IAdaptable)
	 */
	@Override
	public IParser getParser(IAdaptable hint) {
		final String vid = hint.getAdapter(String.class);

		if (vid != null)
			return getParser(CodeCadenzaVisualIDRegistry.getVisualID(vid));

		final View view = hint.getAdapter(View.class);

		if (view != null)
			return getParser(CodeCadenzaVisualIDRegistry.getVisualID(view));

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.common.core.service.IProvider#provides(org.eclipse.gmf.runtime.common.core.service.IOperation)
	 */
	@Override
	public boolean provides(IOperation operation) {
		if (operation instanceof final GetParserOperation parserOperation) {
			final IAdaptable hint = parserOperation.getHint();

			if (CodeCadenzaElementTypes.getElement(hint) == null)
				return false;

			return getParser(hint) != null;
		}

		return false;
	}

	public static class HintAdapter extends ParserHintAdapter {
		private final IElementType elementType;

		/**
		 * @param type
		 * @param object
		 * @param parserHint
		 */
		public HintAdapter(IElementType type, EObject object, String parserHint) {
			super(object, parserHint);

			elementType = type;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.gmf.runtime.emf.ui.services.parser.ParserHintAdapter#getAdapter(java.lang.Class)
		 */
		@Override
		public Object getAdapter(Class adapter) {
			if (IElementType.class.equals(adapter))
				return elementType;

			return super.getAdapter(adapter);
		}
	}

}
