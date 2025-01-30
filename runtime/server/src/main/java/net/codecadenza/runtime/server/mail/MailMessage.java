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
package net.codecadenza.runtime.server.mail;

import java.util.ArrayList;
import java.util.Collection;

/**
 * <p>
 * Data transfer object for mail messages
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MailMessage implements java.io.Serializable {
	private static final long serialVersionUID = 1269852321972549265L;

	private Collection<String> to = new ArrayList<>();
	private Collection<String> cc = new ArrayList<>();
	private String subject = "";
	private String message = "";
	private String attachmentName;
	private byte[] attachmentContent;
	private String fromEmail;
	private String fromName;

	/**
	 * Default constructor
	 */
	public MailMessage() {
	}

	/**
	 * Constructor
	 * @param to
	 * @param subject
	 * @param message
	 */
	public MailMessage(String to, String subject, String message) {
		this.subject = subject;
		this.to.add(to);
		this.message = message;
	}

	/**
	 * @return the carbon copies
	 */
	public Collection<String> getCc() {
		return cc;
	}

	/**
	 * @param cc the carbon copies to set
	 */
	public void setCc(Collection<String> cc) {
		this.cc = cc;
	}

	/**
	 * @return the message
	 */
	public String getMessage() {
		return message;
	}

	/**
	 * @param message the message to set
	 */
	public void setMessage(String message) {
		this.message = message;
	}

	/**
	 * @return the recipients
	 */
	public Collection<String> getTo() {
		return to;
	}

	/**
	 * @param to the recipients to set
	 */
	public void setTo(Collection<String> to) {
		this.to = to;
	}

	/**
	 * @return the subject
	 */
	public String getSubject() {
		return subject;
	}

	/**
	 * @param subject the subject to set
	 */
	public void setSubject(String subject) {
		this.subject = subject;
	}

	/**
	 * @return the attachment name
	 */
	public String getAttachmentName() {
		return attachmentName;
	}

	/**
	 * @param attachmentName the attachment name to set
	 */
	public void setAttachmentName(String attachmentName) {
		this.attachmentName = attachmentName;
	}

	/**
	 * @return the attachment content
	 */
	public byte[] getAttachmentContent() {
		return attachmentContent;
	}

	/**
	 * @param attachmentContent the attachment content to set
	 */
	public void setAttachmentContent(byte[] attachmentContent) {
		this.attachmentContent = attachmentContent;
	}

	/**
	 * @return the email address of the sender
	 */
	public String getFromEmail() {
		return fromEmail;
	}

	/**
	 * @param fromEmail
	 */
	public void setFromEmail(String fromEmail) {
		this.fromEmail = fromEmail;
	}

	/**
	 * @return the name of the sender
	 */
	public String getFromName() {
		return fromName;
	}

	/**
	 * @param fromName
	 */
	public void setFromName(String fromName) {
		this.fromName = fromName;
	}

}
