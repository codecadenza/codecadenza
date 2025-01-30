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
package net.codecadenza.eclipse.tools.jpaeditor.viewer;

import static net.codecadenza.eclipse.shared.Constants.IMG_ATTRIBUTE;
import static net.codecadenza.eclipse.shared.Constants.IMG_DOMAIN_OBJECT;
import static net.codecadenza.eclipse.shared.Constants.IMG_MTO_ASSOC;

import java.util.HashMap;
import java.util.Map;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.tools.util.editor.WordPartDetector;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.swt.graphics.Image;

/**
 * <p>
 * Completion processor for JPA keywords, persistent objects and attributes
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JPACompletionProcessor implements IContentAssistProcessor {
	private final JPASyntax syntax;
	private static final int ENTITY_TYPE = 0;
	private static final int ATTR_TYPE = 1;
	private static final int ASSOC_TYPE = 3;
	private static final int RES_WORD_TYPE = 2;

	/**
	 * Constructor
	 * @param syntax
	 */
	public JPACompletionProcessor(JPASyntax syntax) {
		this.syntax = syntax;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#computeCompletionProposals(org.eclipse.jface.text.
	 * ITextViewer, int)
	 */
	@Override
	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer, int documentOffset) {
		final var proposalList = new HashMap<String, Integer>();
		final var wordPart = new WordPartDetector(viewer, documentOffset);
		ICompletionProposal[] result;
		int index = 0;
		final String text = viewer.getTextWidget().getText().substring(0, documentOffset);
		String token = text.isEmpty() ? text : text.substring(0, text.length() - 1);
		int charPos = -1;
		int lastCharPos = -1;

		for (final char c : token.toCharArray()) {
			charPos++;

			if (!Character.isLetterOrDigit(c))
				lastCharPos = charPos;
		}

		if (lastCharPos != -1)
			token = token.substring(lastCharPos + 1);

		if (syntax.getDomainObjectAttributeMap().containsKey(token)) {
			for (final String word : syntax.getDomainObjectAttributeMap().get(token))
				if (word.toLowerCase().startsWith(wordPart.getString().trim().toLowerCase()))
					proposalList.put(word, ATTR_TYPE);

			for (final String word : syntax.getDomainAssociationMap().get(token))
				if (word.toLowerCase().startsWith(wordPart.getString().trim().toLowerCase()))
					proposalList.put(word, ASSOC_TYPE);
		}
		else {
			final String fullText = viewer.getTextWidget().getText();

			// Check if the word part represents an alias!
			for (final String entityName : syntax.getDomainObjectAttributeMap().keySet()) {
				if (fullText.contains(entityName + " " + token)) {
					syntax.getDomainObjectAttributeMap().get(entityName).forEach(attrName -> proposalList.put(attrName, ATTR_TYPE));
					syntax.getDomainAssociationMap().get(entityName).forEach(assocName -> proposalList.put(assocName, ASSOC_TYPE));
				}
			}

			if (proposalList.isEmpty()) {
				// Iterate over all reserved words
				for (final String word : syntax.getReservedWords())
					if (word.toLowerCase().startsWith(wordPart.getString().trim().toLowerCase()))
						proposalList.put(word.toLowerCase(), RES_WORD_TYPE);

				for (final String word : syntax.getDomainObjectAttributeMap().keySet())
					if (word.toLowerCase().startsWith(wordPart.getString().trim().toLowerCase()))
						proposalList.put(word, ENTITY_TYPE);
			}
		}

		result = new ICompletionProposal[proposalList.size()];

		// Create the proposals
		for (final Map.Entry<String, Integer> entry : proposalList.entrySet()) {
			// Create a new completion proposal
			final int proposalType = entry.getValue();
			final String proposal = entry.getKey();

			Image tmpImage = null;

			if (proposalType == ATTR_TYPE)
				tmpImage = CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE);
			else if (proposalType == ENTITY_TYPE)
				tmpImage = CodeCadenzaResourcePlugin.getImage(IMG_DOMAIN_OBJECT);
			else if (proposalType == ASSOC_TYPE)
				tmpImage = CodeCadenzaResourcePlugin.getImage(IMG_MTO_ASSOC);

			result[index] = new CompletionProposal(proposal, wordPart.getOffset(), wordPart.getString().length(), proposal.length(),
					tmpImage, proposal, null, "");

			index++;
		}

		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getCompletionProposalAutoActivationCharacters()
	 */
	@Override
	public char[] getCompletionProposalAutoActivationCharacters() {
		return new char[] { '.' };
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#computeContextInformation(org.eclipse.jface.text.
	 * ITextViewer, int)
	 */
	@Override
	public IContextInformation[] computeContextInformation(ITextViewer arg0, int arg1) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getContextInformationAutoActivationCharacters()
	 */
	@Override
	public char[] getContextInformationAutoActivationCharacters() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getContextInformationValidator()
	 */
	@Override
	public IContextInformationValidator getContextInformationValidator() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getErrorMessage()
	 */
	@Override
	public String getErrorMessage() {
		return null;
	}

}
